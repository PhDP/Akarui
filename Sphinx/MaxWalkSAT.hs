module Sphinx.MaxWalkSAT where

import Data.Map (Map)
import Data.List (foldl')
import Sphinx.Predicate
import Sphinx.Formula
import Sphinx.MLN

-- Assigns all predicates to a boolean.
type Ass t = Map (Predicate t) Bool

data Status = Success | Failure

-- | The MaxWalkSAT algorithm with a max number of tries (mt), max number
-- of flips (mt), a target cost, a probability of flipping, and a markov
-- logic network of clauses.
-- Reference: ...
maxWalkSAT :: Int -> Int -> Double -> Double -> MLN String -> (Status, Ass String)
maxWalkSAT mt mf target p mln = step mt $ randomFairAss 42 vars
  where
    -- Set of variables
    vars = predicates mln
    -- A single step
    step 0 soln = (Failure, soln)
    step n _    =
      let
        soln = randomFairAss (42 + n) vars
        unsatisfied = filter (unsatisfiable soln . fst) mln
        cost = foldl' (\acc f -> acc + snd f) 0.0 unsatisfied
      in
        if cost <= target then
          (Success, soln)
        else
          (Failure, soln)
