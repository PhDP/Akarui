module Sphinx.MaxWalkSAT where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl', minimumBy)
import Data.Function (on)
import System.Random
import Sphinx.Predicate
import Sphinx.Formula
import Sphinx.MLN

-- | Assigns all predicates to a boolean.
-- type Ass t = Map (Predicate t) Bool

-- | The MaxWalkSAT algorithm with a max number of tries (mt), max number
-- of flips (mt), a target cost, a probability of flipping, and a markov
-- logic network of clauses.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 24.
maxWalkSAT :: Int -> Int -> Double -> Double -> Int -> MLN String -> Maybe (Map (Predicate String) Bool)
maxWalkSAT mt mf target p seed mln = step (mkStdGen seed) mt
  where
    -- Set of atoms (predicates) in the MLN:
    vars = predicates mln
    -- A single step. Returns a Maybe (Ass String)

    step _ 0 = Nothing
    step r n =
      case flips r (randomFairAss (42 + n) vars) mf of
        Just ass -> Just ass
        Nothing  -> step r (n - 1) -- Try again...

    -- To implement
    deltaCost _ = 0.5 :: Double

    -- The 'flip' steps takes a random number generator, a solution, and the flip number:
    flips _ _    0   = Nothing
    flips r s n = if cost <= target then Just s else flips r''' s' (n - 1)
      where
        -- List of unsatisfied formula under soln:
        unsatisfied = filter (unsatisfiable s . fst) mln
        -- Sum of weights of unsatisfied clauses in soln
        cost = foldl' (\acc f -> acc + snd f) 0.0 unsatisfied

        -- Pick a clause randomly among the unsatisfied clauses:
        (idx0, r') = randomR (0, length unsatisfied - 1) r :: (Int, StdGen)
        (c, _) = unsatisfied !! idx0

        vs = atomsLs c

        vlow = fst $ minimumBy (compare `on` snd) (map (\v' -> (v', deltaCost v')) vs)

        (flipTest, r'') = random r' :: (Double, StdGen)
        isFlipping = flipTest < p

        -- Pick an atom randomly in the randomly selected clause:
        (idx1, r''') = randomR (0, length vs - 1) r'' :: (Int, StdGen)
        vf = if isFlipping then vs !! idx1 else vlow

        -- Solution with vf flipped
        s' = Map.adjust not vf s
