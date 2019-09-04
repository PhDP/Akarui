-- | WalkSat algorithms to find the most likely assignments to atoms.
module Faun.WalkSAT
( walkSAT
, maxWalkSAT
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.List (foldl')
import qualified Data.Set as Set
import System.Random
import Faun.Formula
import Faun.Predicate
import Faun.FOL
import Faun.FormulaSet

-- | The WalkSAT algorithm as descripted in Russell and Norvig
-- /Artificial Intelligence 3rd edition/, p 263.
walkSAT
  :: Set FOL -- ^ A set of clauses.
  -> Double -- ^ Probability of flipping.
  -> Int -- ^ Max number of flips before giving up.
  -> Int -- ^ Seed for the random number generator.
  -> Maybe (Map Predicate Bool) -- ^ A (possible) assignment to atoms that satisfies the formula.
walkSAT fs p mf seed = step (mkStdGen seed) m0 mf
  where
    -- All the atoms in the set of formulas:
    a = allAtoms fs
    -- Initial model:
    m0 = randomFairAss (mkStdGen seed) a

    step _ _ 0 = Nothing
    step g m n
      | satisfiesAll m fs = Just m
      | otherwise = step g''' m' (n - 1)
        where
          -- Unsatisfied formulas:
          unsatisfied = Set.toList $ filterUnsatisfied m fs

          -- Pick a clause randomly among the unsatisfied clauses:
          (idx0, g') = randomR (0, length unsatisfied - 1) g :: (Int, StdGen)
          u = unsatisfied !! idx0

          -- Pick a random atom in the unsatisfied clause:
          atoms' = atomsLs u
          (idx1, g'') = randomR (0, length atoms' - 1) g' :: (Int, StdGen)
          a' = atoms' !! idx1

          -- Number of satisfied clauses when flipping each atom in the unsatisfied clause:
          bestFlip =
            fst $
              foldl'
                (\(best, c) x ->
                  let c' = count x in if c' > c then (x, c') else (best, c))
                (head atoms', count $ head atoms')
                (tail atoms')
            where count x = numSatisfied (Map.adjust not x m) fs

          -- Whether a random atom is flipped or the one with the lowest deltaCost flips.
          (flipTest, g''') = random g'' :: (Double, StdGen)
          flips = flipTest < p

          toFlip = if flips then a' else bestFlip

          m' = Map.adjust not toFlip m

-- | The MaxWalkSAT algorithm with a max number of tries (mt), max number
-- of flips (mt), a target cost, a probability of flipping, and a markov
-- logic network of clauses.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 24.
maxWalkSAT
  :: Int -- ^ Number of tries.
  -> Int -- ^ Max number of flips.
  -> Double -- ^ Target cost (sum of the failing formulas).
  -> Double -- ^ Probability of flipping.
  -> Int -- ^ Seed.
  -> Map FOL Double -- ^ Probabilistic knowledge base.
  -> Maybe (Map Predicate Bool) -- ^ The answer (or not).
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

    -- The 'flips' steps take a rng, a solution, and the number of flips left:
    flipStep _ _ 0   = Nothing
    flipStep r s n = if cost <= target then Just s else flipStep r''' s' (n - 1)
      where
        -- List of unsatisfied formula under soln:
        unsatisfied = Map.filterWithKey (\k _ -> unsatisfiable s k) fs
        -- Sum of weights of unsatisfied clauses in soln
        cost = Map.foldr (+) 0.0 unsatisfied

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

    -- Cost of flipping atom v
    deltaCost cost s v = cost' - cost
      where
        unsatisfied' = Map.filterWithKey
          (\k _ -> unsatisfiable (Map.adjust not v s) k) fs
        cost' = Map.foldr (+) 0.0 unsatisfied'
