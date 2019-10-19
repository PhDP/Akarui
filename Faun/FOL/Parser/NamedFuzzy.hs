-- | Faun.Fuzzy is a fun functional set of functions for fuzzy logic
module Faun.FOL.Parser.NamedFuzzy
( parseNamedFuzzy
) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.FOL.Parser.Core
import Faun.FOL.Parser.FuzzySet
import Faun.MVL.NamedFuzzy

-- | Parse a fuzzy set.
parseNamedFuzzy :: String -> Either ParseError NamedFuzzy
parseNamedFuzzy = parse (contents getNamedFuzzy) "<stdin>"

getNamedFuzzy :: Parser NamedFuzzy
getNamedFuzzy = do
  n <- identifier
  reservedOp "="
  reservedOp "{"
  fs <- getFuzzy
  reservedOp "}"
  return $ NamedFuzzy (T.pack n) fs
