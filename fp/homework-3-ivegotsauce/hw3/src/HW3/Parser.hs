{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( parse
  ) where

import Control.Applicative hiding (many)
import Control.Monad.Combinators.Expr
import qualified Data.ByteString as B
import Data.Char
import Data.Functor
import qualified Data.Text as T
import Data.Void
import HW3.Base
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> parseExpr <* eof) []

parseExpr :: Parser HiExpr
parseExpr = makeExprParser parseHiExpr operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*" HiFunMul
    , binaryL'    HiFunDiv
    ]
  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub
    ]
  , [ binaryN "<=" HiFunNotGreaterThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN "==" HiFunEquals
    , binaryN ">" HiFunGreaterThan
    , binaryN "<" HiFunLessThan
    , binaryN "/=" HiFunNotEquals
    ]
  , [ binaryR "&&" HiFunAnd]
  , [ binaryR "||" HiFunOr]
  ]

binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN name f = InfixN (curryHiFun f <$ symbol name)

binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL (curryHiFun f <$ symbol name)

binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR name f = InfixR (curryHiFun f <$ symbol name)

binaryL' :: HiFun -> Operator Parser HiExpr
binaryL' f = InfixL $ (try . lexeme $ char '/' <* notFollowedBy (char '=')) $> curryHiFun f

curryHiFun :: HiFun -> HiExpr -> HiExpr -> HiExpr
curryHiFun f x y = HiExprApply (HiExprValue (HiValueFunction f)) [x, y]

parseHiExpr :: Parser HiExpr
parseHiExpr = try parseHiExprExec <|> try (parenthese parseExpr) <|> parseHiExprValue

parseHiExprExec :: Parser HiExpr
parseHiExprExec = do
  t <- parseHiExprValue
  a <- parseHiExprExec' t
  pure a

parseHiExprExec' :: HiExpr -> Parser HiExpr
parseHiExprExec' val =
  try (parseHiExprApply' val) <|> try (parseHiExprRun' val) <|> parseDotAccess' val

parseHiExprRun' :: HiExpr -> Parser HiExpr
parseHiExprRun' expr = do
  _ <- symbol "!"
  try (parseHiExprExec' (HiExprRun expr)) <|> return (HiExprRun expr)

parseHiExprApply' :: HiExpr -> Parser HiExpr
parseHiExprApply' val = do
  p1 <- (parenthese parseArguments)
  try (parseHiExprExec' (HiExprApply val p1)) <|> (return (HiExprApply val p1))

parseDotAccess' :: HiExpr -> Parser HiExpr
parseDotAccess' val = do
  _ <- char '.'
  lst <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  let ident = (HiExprValue . HiValueString . T.intercalate "-" . (fmap T.pack)) lst
  let e = HiExprApply val [ident]
  try (parseHiExprExec' e) <|> (return e)

parseHiExprValue :: Parser HiExpr
parseHiExprValue = (HiExprValue <$> parseHiValue) <|> parseHiList <|> try (parenthese parseExpr) <|> parseHiDict

parseHiValue :: Parser HiValue
parseHiValue = lexeme $ choice
  [ parseHiString
  , parseHiFun
  , parseHiBool
  , parseHiNull
  , parseHiNumber
  , parseHiBytes
  , parseHiAction
  ]

parseHiList :: Parser HiExpr
parseHiList = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> between (symbol "[") (symbol "]") parseArguments

parseHiDict :: Parser HiExpr
parseHiDict = HiExprDict <$> between (symbol "{") (symbol "}") (parsePair `sepBy` (symbol ","))

parsePair :: Parser (HiExpr, HiExpr)
parsePair = do
  e1 <- parseExpr
  _ <- symbol ":"
  e2 <- parseExpr
  return (e1, e2)

parseHiString :: Parser HiValue
parseHiString = (HiValueString . T.pack) <$> ((char '\"') *> (manyTill L.charLiteral (char '\"')))

parseHiNull :: Parser HiValue
parseHiNull = string "null" $> HiValueNull

parseHiAction :: Parser HiValue
parseHiAction = HiValueAction <$> choice
  [ string "cwd" $> HiActionCwd
  , string "now" $> HiActionNow
  ]

parseHiBool :: Parser HiValue
parseHiBool = HiValueBool <$> choice
  [ string "true" $> True
  , string "false" $> False
  ]

parseHiBytes :: Parser HiValue
parseHiBytes = (HiValueBytes . B.pack . (map fromIntegral)) <$> parseBytes

parseBytes :: Parser [Int]
parseBytes = between (symbol "[#") (symbol "#]") (parseHex `sepEndBy` space1)

parseHex :: Parser Int
parseHex  = do
  c1 <- hexDigitChar
  c2 <- hexDigitChar
  return (16 * digitToInt c1 + digitToInt c2)

parseHiNumber :: Parser HiValue
parseHiNumber = (HiValueNumber . toRational) <$> (L.signed sc L.scientific)

parseHiFun :: Parser HiValue
parseHiFun =  HiValueFunction <$> choice
  [ string "div" $> HiFunDiv
  , string "mul" $> HiFunMul
  , string "add" $> HiFunAdd
  , string "sub" $> HiFunSub
  , string "not-less-than" $> HiFunNotLessThan
  , string "not-greater-than" $> HiFunNotGreaterThan
  , string "not-equals" $> HiFunNotEquals
  , string "not" $> HiFunNot
  , string "and" $> HiFunAnd
  , string "or" $> HiFunOr
  , string "less-than" $> HiFunLessThan
  , string "greater-than" $> HiFunGreaterThan
  , string "equals" $> HiFunEquals
  , string "if" $> HiFunIf
  , string "length" $> HiFunLength
  , string "to-upper" $> HiFunToUpper
  , string "to-lower" $> HiFunToLower
  , string "reverse" $> HiFunReverse
  , string "trim" $> HiFunTrim
  , string "list" $> HiFunList
  , string "range" $> HiFunRange
  , string "fold" $> HiFunFold
  , string "pack-bytes" $> HiFunPackBytes
  , string "unpack-bytes" $> HiFunUnpackBytes
  , string "zip" $> HiFunZip
  , string "unzip" $> HiFunUnzip
  , string "decode-utf8" $> HiFunDecodeUtf8
  , string "encode-utf8" $> HiFunEncodeUtf8
  , string "serialise" $> HiFunSerialise
  , string "deserialise" $> HiFunDeserialise
  , string "read" $> HiFunRead
  , string "write" $> HiFunWrite
  , string "mkdir" $> HiFunMkDir
  , string "cd" $> HiFunChDir
  , string "parse-time" $> HiFunParseTime
  , string "rand" $> HiFunRand
  , string "echo" $> HiFunEcho
  , string "count" $> HiFunCount
  , string "keys" $> HiFunKeys
  , string "values" $> HiFunValues
  , string "invert" $> HiFunInvert
  ]

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseArguments :: Parser [HiExpr]
parseArguments = parseExpr `sepBy` (symbol ",")

parenthese :: Parser a -> Parser a
parenthese = between (symbol "(") (symbol ")")
