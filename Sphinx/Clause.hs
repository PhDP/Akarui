-- | Type and functions for first-order predicate logic.
module Sphinx.Clause where

import Data.List (foldl1')
import Sphinx.Formula
import qualified Data.Set as Set
import Data.Set (Set)

-- | A clause is a disjunction of positive and negative prediates.
data Clause t = Clause (Set t) (Set t)

-- Transforms into a more human-readable formula.
toFormula :: (Ord t) => Clause t -> Formula t
toFormula (Clause ps ns)
  | Set.null ps   = foldl1' (BinOp Or) $ map Atom $ Set.toList ps
  | Set.null ns   = foldl1' (BinOp Or) $ map (Not . Atom) $ Set.toList ns
  | otherwise     =
      BinOp Implies
        (foldl1' (BinOp And) $ map Atom $ Set.toList ns)
        (foldl1' (BinOp Or)  $ map Atom $ Set.toList ps)
