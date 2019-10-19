-- | Type and functions for first-order predicate logic.
module Faun.FOL.SetClause where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Faun.FOL.Clausal
import Faun.FOL.Formula
import Faun.FOL.LiteralSign
import Faun.Utils (sfoldr1')
import Faun.FOL.Parser
import Faun.FOL.Predicate
import Faun.FOL.ParseProbability
import Text.Parsec
import Text.Parsec.String (Parser)

-- | A clause is a disjunction of positive and negative prediates.
data SetClause t = SetClause (Set t) (Set t)

-- | Transforms into a more human-readable formula.
toFormula :: (Ord t) => SetClause t -> Formula t
toFormula (SetClause ps ns)
  | Set.null ps   = sfoldr1' (BinOp Or) $ Set.map (Not . Atom) ns
  | Set.null ns   = sfoldr1' (BinOp Or) $ Set.map Atom ps
  | otherwise     =
      BinOp Implies
        (sfoldr1' (BinOp And) $ Set.map Atom ns)
        (sfoldr1' (BinOp Or)  $ Set.map Atom ps)

-- TODO:: 'find' and 'member' should be in a type class.

-- | Returns either the sign of the literal if found, or Nothing.
find :: (Ord t) => t -> SetClause t -> Maybe LiteralSign
find e (SetClause ps ns)
  | Set.member e ps = Just Positive
  | Set.member e ns = Just Negative
  | otherwise       = Nothing

-- | Checks if a literal is in the clause.
member :: (Ord t) => t -> SetClause t -> Bool
member e (SetClause ps ns) = Set.member e ps || Set.member e ns

instance Clausal (SetClause t) where
  numPositive (SetClause ps _) = Set.size ps
  numNegative (SetClause _ ns) = Set.size ns

-- | Parse a clause (a disjunction of positive and negative literals).
--
-- @
--    !Women(p) or Vegetarian(p)
-- @
parseClause :: String -> Either ParseError (SetClause (Predicate String))
parseClause = parse (contents parseCl) "<stdin>"

-- | Parse a weighted clause (a disjunction of positive and negative literals).
--
-- @
--    1.5 !Women(p) or Vegetarian(p)
--    !Women(p) or Vegetarian(p)  1.5
-- @
parseWClause :: String -> Either ParseError (SetClause (Predicate String), Double)
parseWClause = parse (contents parseWeightedClause) "<stdin>"


parseCl :: Parser (SetClause (Predicate String))
parseCl = do
  optional $ reservedOp "("
  ls <- parsePredTruth `sepBy` (symbol "v" <|> symbol "or" <|> symbol "∨" <|> symbol "|")
  optional $ reservedOp ")"
  let ps = foldl' (\a (p, b) -> if b then Set.insert p a else a) Set.empty ls
      ns = foldl' (\a (p, b) -> if not b then Set.insert p a else a) Set.empty ls in
    return $ SetClause ps ns

-- Parse a weight and then a first-order logic formula
parseLeftWC :: Parser (SetClause (Predicate String), Double)
parseLeftWC  = do
  n <- float
  c <- parseCl
  return (c, n)

-- Parse a first-order logic formula and then a weight
parseRightWC :: Parser (SetClause (Predicate String), Double)
parseRightWC = do
  c <- parseCl
  n <- float
  return (c, n)

parseWeightedClause :: Parser (SetClause (Predicate String), Double)
parseWeightedClause = try parseLeftWC <|> parseRightWC
