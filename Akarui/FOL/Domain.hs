-- | Domain of terms.
module Akarui.FOL.Domain where

import Data.Set (Set)
import Akarui.Parser
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Set as Set

-- | Domain of variables and constants.
data Domain =
    Any
  | Interval Double Double
  | Finite (Set String)

-- | Parse a clause (a disjunction of positive and negative literals).
--
-- @
--    dom1={1, 2, 3, 4}
--    person = { Elaine, George, Jerry, Cosmo, Newman }
-- @
parseDomain :: String -> Either ParseError (String, Set String)
parseDomain = parse (contents parseDs) "<stdin>"


parseDs :: Parser (String, Set String)
parseDs = do
  n <- identifier
  reservedOp "="
  reservedOp "{"
  elems <- commaSep identifier
  reservedOp "}"
  return (n, Set.fromList elems)
