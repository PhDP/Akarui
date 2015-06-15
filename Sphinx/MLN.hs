module Sphinx.MLN where

import qualified Data.Set as Set
import Data.Set (Set)
import Sphinx.FOL
import Sphinx.Formula

type MLN t = [(FOL t, Double)]

-- Gathers all the predicates of a markov logic network in a set.
predicates :: (Ord t) => MLN t -> Set (Predicate t)
predicates mln = Set.unions $ map (atoms . fst) mln
