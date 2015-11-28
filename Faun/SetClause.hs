-- | Type and functions for first-order predicate logic.
module Faun.SetClause where

import qualified Data.Set as Set
import Data.Set (Set)
import Faun.Clausal
import Faun.Formula
import Faun.LiteralSign
import Faun.Utils (sfoldr1')

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
