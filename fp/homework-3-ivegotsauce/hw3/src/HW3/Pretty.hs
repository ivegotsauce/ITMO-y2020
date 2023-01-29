{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as B
import Data.Char
import qualified Data.Map as M
import Data.Ratio
import Data.Scientific
import qualified Data.Sequence as S
import GHC.Word
import HW3.Base
import Prettyprinter.Internal
import Prettyprinter.Render.Terminal

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber r)   = prettyRational r
prettyValue (HiValueFunction f) = prettyFunction f
prettyValue (HiValueBool b)     = pretty $ if b then "true" else "false"
prettyValue HiValueNull         = pretty "null"
prettyValue (HiValueString s)   = viaShow s
prettyValue (HiValueList S.Empty) = pretty "[ ]"
prettyValue (HiValueList s)     = pretty "[" <+> concatWith (surround (pretty ", ")) (fmap prettyValue s) <+> pretty "]"
prettyValue (HiValueBytes b)
  | B.null b = pretty "[# #]"
  | otherwise = pretty "[#" <+> concatWith (surround (pretty " ")) (map prettyByte (B.unpack b)) <+> pretty "#]"
prettyValue (HiValueAction a) = prettyAction a
prettyValue (HiValueTime t) = pretty "parse-time(\"" <> viaShow t <> pretty "\")"
prettyValue (HiValueDict d)
  | M.null d  = pretty "{ }"
  | otherwise = pretty "{" <+> concatWith (surround (pretty ", ")) (map prettyPair (M.toList d)) <+> pretty "}"

prettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
prettyPair (v1, v2) = prettyValue v1 <> pretty ": " <> prettyValue v2

prettyByteString :: B.ByteString -> Doc AnsiStyle
prettyByteString b = pretty "[#" <+> concatWith (surround (pretty " ")) (map prettyByte (B.unpack b)) <+> pretty "#]"

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte x = pretty (intToDigit $ div (fromIntegral x) 16) <> pretty (intToDigit $ mod (fromIntegral x) 16)

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction = pretty . \case
  HiFunAdd            -> "add"
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunSub            -> "sub"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunFold           -> "fold"
  HiFunRange          -> "range"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunMkDir          -> "mkdir"
  HiFunChDir          -> "cd"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionCwd         -> pretty "cwd"
  HiActionNow         -> pretty "now"
  HiActionChDir fp    -> pretty "cd(" <> viaShow fp <> pretty ")"
  HiActionMkDir fp    -> pretty "mkdir(" <> viaShow fp <> pretty ")"
  HiActionRead fp     -> pretty "read(" <> viaShow fp <> pretty ")"
  HiActionWrite fp bs -> pretty "write(" <> viaShow fp <> pretty ", " <> prettyByteString bs <> pretty ")"
  HiActionRand x y    -> pretty "rand(" <> pretty x <> pretty ", " <> pretty y <> pretty ")"
  HiActionEcho t      -> pretty "echo(" <> viaShow t <> pretty ")"

prettyRational :: Rational -> Doc AnsiStyle
prettyRational r =
  (case (quotRem num denom, fromRationalRepetendUnlimited r) of
     ((_, 0), _)       -> pretty $ div num denom
     (_, (_, Nothing)) -> prettyDec r
     ((0, _), _)       -> pretty num <> pretty '/' <> pretty denom
     ((int, frac), _)  -> pretty int <+> pretty (sgn int) <+>
       prettyRational ((toRational $ abs frac) / (toRational denom)))
       where
         num = numerator r
         denom = denominator r
         sgn x = if x >= 0 then '+' else '-'

prettyDec :: Rational -> Doc AnsiStyle
prettyDec rat = pretty $ sgn num ++ (shows d ("." ++ helper next))
  where
    sgn x = if x >= 0 then "" else "-"
    num = numerator rat
    den = denominator rat
    (d, next) = quotRem (abs num) den
    helper 0 = ""
    helper x = let (d', next') = quotRem (10 * x) den
      in shows d' (helper next')
