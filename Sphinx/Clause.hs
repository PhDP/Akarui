-- | Type and functions for first-order predicate logic.
module Sphinx.Clause where

import Sphinx.Formula
import Sphinx.Utils (sfoldl1')
import qualified Data.Set as Set
import Data.Set (Set)

-- | A clause is a disjunction of positive and negative prediates.
data Clause t = Clause (Set t) (Set t)

-- Transforms into a more human-readable formula.
toFormula :: (Ord t) => Clause t -> Formula t
toFormula (Clause ps ns)
  | Set.null ps   = sfoldl1' (BinOp Or) $ Set.map (Not . Atom) ns
  | Set.null ns   = sfoldl1' (BinOp Or) $ Set.map Atom ps
  | otherwise     =
      BinOp Implies
        (sfoldl1' (BinOp And) $ Set.map Atom ns)
        (sfoldl1' (BinOp Or)  $ Set.map Atom ps)
