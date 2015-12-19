-- | Faun.Fuzzy is a fun functional set of functions for fuzzy logic
module Faun.Parser.FuzzySet
( parseFuzzySet
, getFuzzy
, getFuzzyElement
) where

import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.Parser.Core
import Faun.FuzzySet

-- | Parse a fuzzy set.
parseFuzzySet :: String -> Either ParseError FuzzySet
parseFuzzySet = parse (contents getFuzzy) "<stdin>"

getFuzzy :: Parser FuzzySet
getFuzzy = do
  reservedOp "{"
  elems <- commaSep getFuzzyElement
  reservedOp "}"
  return $ FuzzySet $ foldl' (\m e -> Map.insert (T.pack $ fst e) (snd e) m) Map.empty elems

getFuzzyElement :: Parser (String, Double)
getFuzzyElement = do
  n <- identifier
  reservedOp "/"
  degree <- float
  return (n, degree)
