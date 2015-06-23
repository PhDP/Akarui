-- | WalkSat algorithms to find the most likely assignments to atoms.
module Sphinx.WalkSAT where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random
import Sphinx.Formula

-- | The MaxWalkSAT algorithm with a max number of tries (mt), max number
-- of flips (mt), a target cost, a probability of flipping, and a markov
-- logic network of clauses.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 24.
maxWalkSAT :: Ord a => Int -> Int -> Double -> Double -> Int -> Map (Formula a) Double -> Maybe (Map a Bool)
maxWalkSAT mt mf target p seed fs = step (mkStdGen seed) mt
  where
    -- Set of atoms (predicates) in the MLN:
    vars = Set.unions $ map atoms (Map.keys fs)

    -- A single step. Returns a Maybe type with Nothing if no assignment with
    -- a cost lower than the target has been found.
    step _ 0 = Nothing
    step r n =
      let
        (seed0, r') = random r :: (Int, StdGen)
        (seed1, _) = random r' :: (Int, StdGen)
      in
        case flipStep r (randomFairAss (mkStdGen seed0) vars) mf of
          Just ass -> Just ass
          Nothing  -> step (mkStdGen seed1) (n - 1) -- Try again...

    -- Cost of flipping atom v
    deltaCost cost s v = cost' - cost
      where
        unsatisfied' = Map.filterWithKey (\k _ -> unsatisfiable (Map.adjust not v s) k) fs
        cost' = Map.fold (+) 0.0 unsatisfied'

    -- The 'flips' steps take a rng, a solution, and the number of flips left:
    flipStep _ _ 0   = Nothing
    flipStep r s n = if cost <= target then Just s else flipStep r''' s' (n - 1)
      where
        -- List of unsatisfied formula under soln:
        unsatisfied = Map.filterWithKey (\k _ -> unsatisfiable s k) fs
        -- Sum of weights of unsatisfied clauses in soln
        cost = Map.fold (+) 0.0 unsatisfied

        -- Pick a clause randomly among the unsatisfied clauses:
        (idx0, r') = randomR (0, Map.size unsatisfied - 1) r :: (Int, StdGen)
        (c, _) = Map.elemAt idx0 unsatisfied

        -- Maps deltaCosts to atoms in the selected clause c:
        da = Map.fromList $ map (\a -> (deltaCost cost s a, a)) (atomsLs c)

        -- Whether a random atom is flipped or the one with the lowest deltaCost flips.
        (flipTest, r'') = random r' :: (Double, StdGen)
        flips = flipTest < p

        -- Pick an atom randomly in the randomly selected clause:
        (idx1, r''') = randomR (0, Map.size da - 1) r'' :: (Int, StdGen)
        a' = snd $ if flips then Map.elemAt idx1 da else Map.findMin da

        -- Solution with vf flipped
        s' = Map.adjust not a' s
