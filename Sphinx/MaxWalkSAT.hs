module Sphinx.MaxWalkSAT where

import Data.Map (Map)
import Data.List (foldl')
import Sphinx.Predicate
import Sphinx.Formula
import Sphinx.MLN

-- Assigns all predicates to a boolean.
type Ass t = Map (Predicate t) Bool

maxWalkSAT :: Int -> Int -> Double -> Double -> MLN String -> Ass String
maxWalkSAT mt mf target p mln = step mt ass0
  where
    -- Set of variables
    vars = predicates mln
    -- Initial assignment
    ass0 = randomFairAss 42 vars
    step 0 soln = soln
    step n _    =
      let
        soln = randomFairAss (42 + n) vars
        unsatisfied = filter (unsatisfiable soln . fst) mln
        cost = foldl' (\acc f -> acc + snd f) 0.0 unsatisfied
      in
        if (cost <= target) then
          soln
        else
          soln
