-- | A knowledge base is a set of formulas. See MLN.hs for probabilistic
-- knowledge bases.
module Faun.FormulaSet where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Faun.Formula
import Faun.FOL
import Faun.Predicate
import Faun.Term
import Faun.Parser.FOL

---- | Pretty print a knowledge base.
--showKB :: (Show a) => Set (Formula a) -> String
--showKB = Set.foldr' (\k acc -> show k ++ "\n" ++ acc) ""

-- | Gathers all atoms from a set of formulas.
allAtoms :: (Ord a) => Set (Formula a) -> Set a
allAtoms = Set.foldl' (\acc f -> Set.union acc (atoms f)) Set.empty

-- | Get all groundings from a first-order logic knowledge base.
allGroundings :: [Term] -> Set FOL -> Set FOL
allGroundings ts =
  Set.foldr' (\gs acc -> Set.union (groundings ts gs) acc) Set.empty

-- | Gathers all the predicates of a markov logic network in a set.
allPredicates :: Set FOL -> Set Predicate
allPredicates = Set.foldr' (\k acc -> Set.union (atoms k) acc) Set.empty

-- | Tests if a valuation satisfied a set of formulas.
satisfiesAll :: Map Predicate Bool -> Set FOL -> Bool
satisfiesAll ass = Set.foldl' (\t f -> t && satisfy ass f) True

-- | Filters the formula that are satisfied by a valuation.
filterSatisfied :: Map Predicate Bool -> Set FOL -> Set FOL
filterSatisfied ass = Set.filter (satisfy ass)

-- | Filters the formula that are not satisfied by a valuation.
filterUnsatisfied :: Map Predicate Bool -> Set FOL -> Set FOL
filterUnsatisfied ass = Set.filter (unsatisfiable ass)

-- | Number of satisfied formulas for a given valuation.
numSatisfied :: Map Predicate Bool -> Set FOL -> Int
numSatisfied ass = Set.foldl' (\n f -> n + if satisfy ass f then 1 else 0) 0

-- | All valuations that are true for a set of formulas.
trueValuations :: Set FOL -> [Map Predicate Bool]
trueValuations fs = filter (`satisfiesAll` fs) (allAss fs)

-- | Builds a knowledge base from a list of strings. If the parser fails
-- to parse a formula, it is ignored.
fromStrings :: [String] -> Set FOL
fromStrings = foldr
  (\k acc ->
    case parseFOL k of
      Left _  -> acc
      Right f -> Set.insert f acc)
  Set.empty
