-- | Type and functions for first-order predicate logic.
module Faun.Clause where

import Faun.Formula
import Faun.Utils (sfoldr1')
import qualified Data.Set as Set
import Data.Set (Set)

-- | A clause is a disjunction of positive and negative prediates.
data Clause t = Clause (Set t) (Set t)

definite, fact, rule, query, horn :: Clause t -> Bool

definite (Clause ps _) = Set.size ps == 1
fact (Clause ps ns) = Set.size ps == 1 && Set.size ns == 0
rule (Clause ps ns) = Set.size ps == 1 && Set.size ns > 0
query (Clause ps ns) = Set.size ps == 0 && Set.size ns > 0
horn (Clause ps _) = Set.size ps < 2

-- Transforms into a more human-readable formula.
toFormula :: (Ord t) => Clause t -> Formula t
toFormula (Clause ps ns)
  | Set.null ps   = sfoldr1' (BinOp Or) $ Set.map (Not . Atom) ns
  | Set.null ns   = sfoldr1' (BinOp Or) $ Set.map Atom ps
  | otherwise     =
      BinOp Implies
        (sfoldr1' (BinOp And) $ Set.map Atom ns)
        (sfoldr1' (BinOp Or)  $ Set.map Atom ps)
