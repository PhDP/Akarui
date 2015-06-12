module Manticore.MaxWalkSAT where

import Manticore.FOL
--import Data.Map (Map)

data Ass t = Map (Predicate t) Bool

maxWalkSAT :: Int -> Ass t -> Ass t
maxWalkSAT 0     c = c
maxWalkSAT tries c =
  maxWalkSAT (tries - 1) c
