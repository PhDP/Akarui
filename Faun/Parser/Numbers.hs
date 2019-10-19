module Faun.Parser.Numbers
( parseDouble
, getDouble
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.Parser.Core
import qualified Text.Parsec.Token as Tok

parseDouble :: String -> Either ParseError Double
parseDouble = parse (contents getDouble) "<stdin>"

getDouble, ndouble, pdouble, int :: Parser Double

getDouble = try ndouble <|> try pdouble <|> int

int = do
  i <- Tok.integer lexer
  return $ fromIntegral i

ndouble = do
  reservedOp "-"
  f <- Tok.float lexer
  return (-f)

pdouble = do
  optional $ reservedOp "+"
  Tok.float lexer
