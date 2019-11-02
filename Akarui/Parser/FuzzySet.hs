-- | Akarui.Fuzzy is a fun functional set of functions for fuzzy logic
module Akarui.Parser.FuzzySet
( parseFuzzySet
, getFuzzy
, getFuzzyElement
) where

import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Akarui.Parser.Core
import Akarui.Parser.Numbers
import Akarui.MVL.FuzzySet
import Akarui.MVL.Fuzzy

-- | Parse a fuzzy set.
parseFuzzySet :: String -> Either ParseError (FuzzySet T.Text)
parseFuzzySet = parse (contents getFuzzy) "<stdin>"

getFuzzy :: Parser (FuzzySet T.Text)
getFuzzy = do
  reservedOp "{"
  elems <- commaSep getFuzzyElement
  reservedOp "}"
  return $ MapFS $ foldl' (\m e -> Map.insert (T.pack $ fst e) (mkFuzzy $ snd e) m) Map.empty elems

getFuzzyElement :: Parser (String, Double)
getFuzzyElement = do
  n <- identifier
  reservedOp "/"
  degree <- getDouble
  return (n, degree)
