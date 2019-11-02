-- | Akarui.Fuzzy is a fun functional set of functions for fuzzy logic
module Akarui.Parser.NamedFuzzy
( parseNamedFuzzy
) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Akarui.Parser.Core
import Akarui.Parser.FuzzySet
import Akarui.MVL.NamedFuzzy

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
