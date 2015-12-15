-- | Faun.Fuzzy is a fun functional set of functions for fuzzy logic
module Faun.Parser.FuzzySet where

import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.Parser.Core
import Faun.FuzzySet

-- | Parse a fuzzy set.
parseFuzzySet :: String -> Either ParseError FuzzySet
parseFuzzySet = parse (contents parserFuzzy) "<stdin>"

parserFuzzy :: Parser FuzzySet
parserFuzzy = do
  reservedOp "{"
  elems <- commaSep parserFuzzyElement
  reservedOp "}"
  return $ FuzzySet $ foldl' (\m e -> Map.insert (T.pack $ fst e) (snd e) m) Map.empty elems

parserFuzzyElement :: Parser (String, Double)
parserFuzzyElement = do
  name <- identifier
  reservedOp "/"
  degree <- float
  return (name, degree)
