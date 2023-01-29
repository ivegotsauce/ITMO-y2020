{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio
import Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time
import HW3.Base
import System.FilePath
import Text.Read (readMaybe)

data Arity = Unary | Binary | Ternary | Some | Unknown

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evaluate expr

evaluate :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evaluate (HiExprValue val) = return val
evaluate (HiExprApply f args) = do
  ef <- evaluate f
  evalFunction ef args (getArity' ef)
evaluate (HiExprRun r) = evalAction r
evaluate (HiExprDict d) = evalDict d

evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evalDict lst = do
  el <- mapM evalPair lst
  return $ HiValueDict $ M.fromList el

evalPair :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalPair (v1, v2) = do
  ev1 <- evaluate v1
  ev2 <- evaluate v2
  return (ev1, ev2)

evalAction :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalAction a = do
  ea <- evaluate a
  case ea of
    HiValueAction ac -> lift (runAction ac)
    _                -> throwE HiErrorInvalidArgument

evalFunction :: HiMonad m => HiValue -> [HiExpr] -> Arity -> ExceptT HiError m HiValue
evalFunction s args Some                      = evalSome s args
evalFunction (HiValueFunction f) args Unary   = evalUnary f args
evalFunction (HiValueFunction f) args Binary  = evalBinary f args
evalFunction (HiValueFunction f) args Ternary = evalTernary f args
evalFunction _ _ _                            = throwE HiErrorInvalidFunction

evalSome :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalSome op args = do
  ea <- mapM evaluate args
  case (op, ea) of
    (HiValueString s, [x])            -> index (T.unpack s) x (HiValueString . T.pack . (: []))
    (HiValueList l, [x])              -> index (toList l) x id
    (HiValueString s, [x, y])         -> slice (T.unpack s) x y (HiValueString . T.pack)
    (HiValueList l, [x, y])           -> slice (toList l) x y (HiValueList . S.fromList)
    (HiValueBytes b, [x])             -> index (B.unpack b) x (HiValueNumber . toRational @Integer . fromIntegral)
    (HiValueBytes b, [x, y])          -> slice (B.unpack b) x y (HiValueBytes . B.pack)
    (HiValueFunction HiFunList, vals) -> return (HiValueList $ S.fromList vals)
    (HiValueDict d, [x])              -> return $ M.findWithDefault HiValueNull x d
    _                                 -> throwE HiErrorArityMismatch

slice :: HiMonad m => [a1] -> HiValue -> HiValue -> ([a1] -> a2) -> ExceptT HiError m a2
slice s HiValueNull HiValueNull cons = return (cons s)
slice s HiValueNull i cons = slice s (HiValueNumber 0) i cons
slice s i HiValueNull cons = slice s i (HiValueNumber $ toRational (L.length s)) cons
slice s (HiValueNumber i) (HiValueNumber j) cons = case (rationalToInt i, rationalToInt j) of
  (Right i1, Right j1) -> (return . cons . slice' i1 j1) s
  _                    -> throwE HiErrorInvalidArgument
slice _ _ _ _ = throwE HiErrorInvalidArgument

slice' :: Int -> Int -> [a] -> [a]
slice' l r a
  | l >= 0 && r >= 0 = (L.take (r - l) . L.drop l) a
  | l < 0 && r < 0 = (dropLastN (abs r) . takeLastN (abs l)) a
  | l < 0 && r >= 0 = (takeLastN (abs l) . dropLastN (L.length a - r)) a
  | otherwise = (dropLastN (abs r) . L.drop l) a

takeLastN :: Int -> [a] -> [a]
takeLastN n xs = helper (drop n xs) xs
  where
    helper []     []     = []
    helper ps     []     = ps
    helper []     qs     = qs
    helper (_:ps) (_:qs) = helper ps qs

dropLastN :: Int -> [a] -> [a]
dropLastN n = snd . foldr helper (n, [])
  where
    helper x (n', lst)
      | n' > 0 = (n' - 1, [])
      | otherwise = (0, x : lst)

index :: HiMonad m => [a] -> HiValue -> (a -> HiValue) -> ExceptT HiError m HiValue
index s (HiValueNumber i) cons
  | i < 0 || i >= len = return HiValueNull
  | otherwise = case rationalToInt i of
      Left err  -> throwE err
      Right ind -> return (cons $ s !! ind)
  where len = toRational (L.length s)
index _ _ _ = throwE HiErrorInvalidArgument

evalUnary :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalUnary f [x] = do
  ex <- evaluate x
  computeUnary f ex
evalUnary _ _ = throwE HiErrorArityMismatch

evalBinary :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalBinary HiFunAnd [x, y] = do
  ex <- evaluate x
  case ex of
    HiValueBool False -> return ex
    HiValueNull       -> return ex
    _                 -> evaluate y
evalBinary HiFunOr [x, y] = do
  ex <- evaluate x
  case ex of
    HiValueBool False -> evaluate y
    HiValueNull       -> evaluate y
    _                 -> return ex
evalBinary f [x, y] = do
  ex <- evaluate x
  ey <- evaluate y
  computeBinary f ex ey
evalBinary _ _ = throwE HiErrorArityMismatch

evalTernary :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalTernary _ [x, y, z] = do
  ex <- evaluate x
  case ex of
    (HiValueBool True)  -> evaluate y
    (HiValueBool False) -> evaluate z
    _                   -> throwE HiErrorInvalidArgument
evalTernary _ _ = throwE HiErrorArityMismatch

getArity' :: HiValue -> Arity
getArity' (HiValueFunction f) = getArity f
getArity' (HiValueDict _)     = Some
getArity' (HiValueString _)   = Some
getArity' (HiValueList _)     = Some
getArity' (HiValueBytes _)    = Some
getArity' _                   = Unknown

getArity :: HiFun -> Arity
getArity = \case
  HiFunNot            -> Unary
  HiFunLength         -> Unary
  HiFunToUpper        -> Unary
  HiFunToLower        -> Unary
  HiFunReverse        -> Unary
  HiFunTrim           -> Unary
  HiFunPackBytes      -> Unary
  HiFunUnpackBytes    -> Unary
  HiFunEncodeUtf8     -> Unary
  HiFunDecodeUtf8     -> Unary
  HiFunZip            -> Unary
  HiFunUnzip          -> Unary
  HiFunSerialise      -> Unary
  HiFunDeserialise    -> Unary
  HiFunRead           -> Unary
  HiFunMkDir          -> Unary
  HiFunChDir          -> Unary
  HiFunParseTime      -> Unary
  HiFunEcho           -> Unary
  HiFunCount          -> Unary
  HiFunKeys           -> Unary
  HiFunValues         -> Unary
  HiFunInvert         -> Unary
  HiFunAdd            -> Binary
  HiFunAnd            -> Binary
  HiFunDiv            -> Binary
  HiFunEquals         -> Binary
  HiFunGreaterThan    -> Binary
  HiFunLessThan       -> Binary
  HiFunMul            -> Binary
  HiFunNotEquals      -> Binary
  HiFunNotGreaterThan -> Binary
  HiFunNotLessThan    -> Binary
  HiFunOr             -> Binary
  HiFunSub            -> Binary
  HiFunRange          -> Binary
  HiFunFold           -> Binary
  HiFunWrite          -> Binary
  HiFunRand           -> Binary
  HiFunIf             -> Ternary
  HiFunList           -> Some

computeUnary :: HiMonad m => HiFun -> HiValue -> ExceptT HiError m HiValue
computeUnary HiFunNot (HiValueBool b)       = return $ HiValueBool (not b)
computeUnary HiFunLength (HiValueString s)  = return $ (HiValueNumber . toRational) (T.length s)
computeUnary HiFunLength (HiValueList l)    = return $ (HiValueNumber . toRational) (S.length l)
computeUnary HiFunLength (HiValueBytes b)   = return $ (HiValueNumber . toRational) (B.length b)
computeUnary HiFunToUpper (HiValueString s) = return $ HiValueString (T.toUpper s)
computeUnary HiFunToLower (HiValueString s) = return $ HiValueString (T.toLower s)
computeUnary HiFunReverse (HiValueString s) = return $ HiValueString (T.reverse s)
computeUnary HiFunReverse (HiValueList l)   = return $ HiValueList (S.reverse l)
computeUnary HiFunReverse (HiValueBytes b)  = return $ HiValueBytes (B.reverse b)
computeUnary HiFunTrim (HiValueString s)    = return $ HiValueString (T.strip s)
computeUnary HiFunPackBytes (HiValueList l) = HiValueBytes <$> B.pack <$> ((mapM fromIntegral') . toList) l
computeUnary HiFunUnpackBytes (HiValueBytes b) =
  return $ (HiValueList . S.fromList . (map (HiValueNumber . toRational @Integer . fromIntegral)) . B.unpack) b
computeUnary HiFunEncodeUtf8 (HiValueString s) = return $ (HiValueBytes $ encodeUtf8 s)
computeUnary HiFunDecodeUtf8 (HiValueBytes b) = return $ (either (const HiValueNull) HiValueString (decodeUtf8' b))
computeUnary HiFunZip (HiValueBytes b) =
  (return . HiValueBytes . toStrict . (compressWith defaultCompressParams { compressLevel = bestCompression }) . fromStrict) b
computeUnary HiFunUnzip (HiValueBytes b) =
  (return . HiValueBytes . toStrict . (decompressWith defaultDecompressParams) . fromStrict) b
computeUnary HiFunSerialise h = (return . HiValueBytes . toStrict . serialise) h
computeUnary HiFunDeserialise (HiValueBytes b) = deserialise b
computeUnary HiFunRead (HiValueString s) = return $ HiValueAction $ HiActionRead $ T.unpack s
computeUnary HiFunChDir (HiValueString s) = return $ HiValueAction $ HiActionChDir $ T.unpack s
computeUnary HiFunMkDir (HiValueString s) = return $ HiValueAction $ HiActionMkDir $ T.unpack s
computeUnary HiFunParseTime (HiValueString s) = return $ maybe HiValueNull HiValueTime ((readMaybe $ T.unpack s) :: Maybe UTCTime)
computeUnary HiFunEcho (HiValueString s) = return $ HiValueAction $ HiActionEcho s
computeUnary HiFunKeys (HiValueDict d) = return $ HiValueList $ S.fromList $ M.keys d
computeUnary HiFunValues (HiValueDict d) = return $ HiValueList $ S.fromList $ M.elems d
computeUnary HiFunInvert (HiValueDict d) =
  return $ HiValueDict $ M.mapMaybe (Just . HiValueList . S.fromList) $ M.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, v) <- M.toList d]
computeUnary HiFunCount (HiValueString s) = count (map (HiValueString . T.pack . (: [])) (T.unpack s))
computeUnary HiFunCount (HiValueBytes b) = count (map (HiValueNumber . toRational @Integer . fromIntegral) (B.unpack b))
computeUnary HiFunCount (HiValueList l) = count (toList l)
computeUnary _ _                            = throwE HiErrorInvalidArgument

count :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
count = return . HiValueDict . M.mapMaybe (Just . HiValueNumber . toRational @Integer) . M.fromListWith (+) . map (, 1)

deserialise :: HiMonad m => B.ByteString -> ExceptT HiError m HiValue
deserialise b = case deserialiseOrFail (fromStrict b) of
  Left _  -> throwE HiErrorInvalidArgument
  Right r -> return r

fromIntegral' :: (HiMonad m, Num a) => HiValue -> ExceptT HiError m a
fromIntegral' (HiValueNumber r)
  | denom == 1 && num >= 0 && num <= 255 = return $ fromIntegral (numerator r)
  | otherwise = throwE HiErrorInvalidArgument
  where
    denom = denominator r
    num   = numerator r
fromIntegral' _ = throwE HiErrorInvalidArgument

computeBinary :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
computeBinary HiFunDiv (HiValueNumber _) (HiValueNumber 0) = throwE HiErrorDivideByZero
computeBinary HiFunDiv (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x / y)
computeBinary HiFunDiv (HiValueString x) (HiValueString y) = return $ HiValueString (T.pack $ (T.unpack x) </> (T.unpack y))
computeBinary HiFunMul (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x * y)
computeBinary HiFunMul (HiValueString x) (HiValueNumber y) = dublicate x y HiValueString
computeBinary HiFunMul (HiValueList x) (HiValueNumber y) = dublicate x y HiValueList
computeBinary HiFunMul (HiValueBytes x) (HiValueNumber y) = dublicate x y HiValueBytes
computeBinary HiFunAdd (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x + y)
computeBinary HiFunAdd (HiValueString x) (HiValueString y) = return $ HiValueString (x <> y)
computeBinary HiFunAdd (HiValueList x) (HiValueList y) = return $ HiValueList (x <> y)
computeBinary HiFunAdd (HiValueBytes x) (HiValueBytes y) = return $ HiValueBytes (x <> y)
computeBinary HiFunAdd (HiValueTime t) (HiValueNumber r) = return $ HiValueTime (addUTCTime (fromRational r) t)
computeBinary HiFunSub (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x - y)
computeBinary HiFunSub (HiValueTime x) (HiValueTime y) = return $ (HiValueNumber . toRational) (diffUTCTime x y)
computeBinary HiFunAnd (HiValueBool x) (HiValueBool y)     = return $ HiValueBool (x && y)
computeBinary HiFunOr (HiValueBool x) (HiValueBool y)      = return $ HiValueBool (x || y)
computeBinary HiFunLessThan x y                            = return $ HiValueBool (x < y)
computeBinary HiFunGreaterThan x y                         = return $ HiValueBool (x > y)
computeBinary HiFunEquals x y                              = return $ HiValueBool (x == y)
computeBinary HiFunNotLessThan x y                         = return $ HiValueBool (x >= y)
computeBinary HiFunNotGreaterThan x y                      = return $ HiValueBool (x <= y)
computeBinary HiFunNotEquals x y                           = return $ HiValueBool (x /= y)
computeBinary HiFunRange (HiValueNumber x) (HiValueNumber y) = (return . HiValueList . S.fromList) (HiValueNumber <$> [x..y])
computeBinary HiFunFold f (HiValueList l) = case getArity' f of
  Binary -> computeFold f (toList l)
  Some   -> computeFold f (toList l)
  _      -> throwE HiErrorInvalidArgument
computeBinary HiFunWrite (HiValueString fp) (HiValueString s) =
  return $ HiValueAction $ HiActionWrite (T.unpack fp) (encodeUtf8 s)
computeBinary HiFunRand (HiValueNumber x) (HiValueNumber y) = case (rationalToInt x, rationalToInt y) of
  (Right a, Right b) -> return $ HiValueAction $ HiActionRand a b
  _                  -> throwE HiErrorInvalidArgument
computeBinary _ _ _                                        = throwE HiErrorInvalidArgument

computeFold :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
computeFold _ [] = return HiValueNull
computeFold f lst = foldl1 (\x y -> do
  x' <- x
  y' <- y
  evaluate $ HiExprApply (HiExprValue f) [HiExprValue x', HiExprValue y']) (map return lst)

dublicate :: (HiMonad m, Semigroup t) => t -> Rational -> (t -> a) -> ExceptT HiError m a
dublicate s n cons
  | n <= 0 = throwE HiErrorInvalidArgument
  | otherwise = case rationalToInt n of
      Left err  -> throwE err
      Right num -> return $ cons (stimes num s)

rationalToInt :: Rational -> Either HiError Int
rationalToInt n
  | denominator n /= 1 || num > toInteger (maxBound :: Int) || num < toInteger (minBound :: Int) = Left HiErrorInvalidArgument
  | otherwise = Right $ fromInteger num
  where num = numerator n
