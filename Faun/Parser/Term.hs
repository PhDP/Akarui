-- | Parsers for first-order logic and other important structures (e.g. Markov
-- logic networks).
module Faun.Parser.Term (
  parseFunForm,
  parseTerm
) where

import Data.Char (isLower)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.Parser.Core
import Faun.FOL.Term

-- | Parse function-like objects of the form Name(args0, args1, args2, ...).
parseFunForm :: Parser (T.Text, [Term])
parseFunForm = do
  n <- identifier
  reservedOp "("
  ts <- commaSep parseTerm
  reservedOp ")"
  return (T.pack n, ts)

-- | Parse basic terms.
parseTerm, parseVarCon, parseFunction :: Parser Term
parseTerm = try parseFunction <|> parseVarCon

parseFunction = do
  args <- parseFunForm
  return $ uncurry Function args

parseVarCon = do
  n <- identifier
  return $ (if isLower $ head n then Variable else Constant) (T.pack n)
