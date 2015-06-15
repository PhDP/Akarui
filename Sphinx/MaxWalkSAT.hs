module Sphinx.MaxWalkSAT where

import Sphinx.FOL
--import Data.Map (Map)

data Ass t = Map (Predicate t) Bool

--maxWalkSAT :: MLN String -> Int -> Int -> Double -> Double -> Ass t
--maxWalkSAT mln mt mf target p =
--  where
--    vars = predicates mln
--    step
