{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..)
  , Parser (..)
  , pAbbr
  , pChar
  , pEof
  , parseError
  , parseExpr
  , runP
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import HW2.T1 hiding (Pair (..))
import HW2.T4
import HW2.T5
import Numeric.Natural

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES f)) s =
  case f (0, s) of
    Error err        -> Error err
    Success (a :# _) -> Success a

-- Returns ErrorAtPos if the string is empty.
-- Otherwise, returns current char, annotated with pair of incremented position
-- and tail of given string.
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (P (ES f)) <|> (P (ES g)) = P $ ES $ \ps ->
    case f ps of
      Success a -> Success a
      Error _   -> g ps

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (space *> pExpr <* pEof)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = mfilter p pChar

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser String
space = many (satisfy Data.Char.isSpace)

pExpr :: Parser Expr
pExpr = pExprImpl pTerm pExpr'

pExpr' :: Expr -> Parser Expr
pExpr' prev = pExprImpl' prev '+' '-' pTerm pExpr' Add Sub

pTerm :: Parser Expr
pTerm = pExprImpl pFactor pTerm'

pTerm' :: Expr -> Parser Expr
pTerm' prev = pExprImpl' prev '*' '/' pFactor pTerm' Mul Div

pExprImpl :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
pExprImpl p1 p2 = do
  void space
  a <- p1
  void space
  p2 a <|> return a

pExprImpl'
  :: Expr
  -> Char
  -> Char
  -> Parser Expr
  -> (Expr -> Parser Expr)
  -> (Expr -> Expr -> Prim Expr)
  -> (Expr -> Expr -> Prim Expr)
  -> Parser Expr
pExprImpl' prev op1 op2 p1 p2 c1 c2 = do
  void space
  op <- (char op1 <|> char op2)
  void space
  a <- p1
  void space
  e <- getExpr op op1 op2 c1 c2 prev a
  p2 e <|> return e

getExpr
  :: Char
  -> Char
  -> Char
  -> (Expr -> Expr -> Prim Expr)
  -> (Expr -> Expr -> Prim Expr)
  -> Expr
  -> Expr
  -> Parser Expr
getExpr op op1 op2 c1 c2 e1 e2
    | op == op1 = return $ Op (c1 e1 e2)
    | op == op2 = return $ Op (c2 e1 e2)
    | otherwise = parseError

pFactor :: Parser Expr
pFactor = ((char '(' *> pExpr <* char ')') <|> Val <$> pVal) <* space

pVal :: Parser Double
pVal = pRational <|> (fromRational . getRational) <$> pDigits

pRational :: Parser Double
pRational = do
  int <- pDigits
  _ <- char '.'
  frac <- pDigits
  return $ fromRational $ (getRational (int ++ frac)) / (10 ^ (length frac))

getRational :: String -> Rational
getRational s = foldl (\acc val -> acc * 10 + toRational (digitToInt val)) 0 s

pDigits :: Parser String
pDigits = some (satisfy Data.Char.isDigit)
