-- | A knowledge base is a set of formulas. See MLN.hs for probabilistic
-- knowledge bases.
module Manticore.KB where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Manticore.Formula
import Manticore.FOL
import Manticore.Parser
import Manticore.Predicate
import Manticore.Term

-- | A knowledge base is a set of formula.
type KB a = Set (Formula a)

-- | Pretty print a knowledge base.
showKB :: (Show a) => KB a -> String
showKB = Set.foldr' (\k acc -> show k ++ "\n" ++ acc) ""

-- | Gathers all atoms from a set of formulas.
allAtoms :: (Ord a) => Set (Formula a) -> Set a
allAtoms = Set.foldl' (\acc f -> Set.union acc (atoms f)) Set.empty

-- | Get all groundings from a first-order logic knowledge base.
allGroundings :: Map (String, [Term String]) (Term String) -> [Term String] -> KB (Predicate String) -> KB (Predicate String)
allGroundings m ts =
  Set.foldr' (\gs acc -> Set.union (groundings m ts gs) acc) Set.empty

-- | Gathers all the predicates of a markov logic network in a set.
allPredicates :: (Ord t) => KB (Predicate t) -> Set (Predicate t)
allPredicates = Set.foldr' (\k acc -> Set.union (atoms k) acc) Set.empty

-- | Tests if a valuation satisfied a set of formulas.
satisfiesAll :: (Ord a) => Map a Bool -> Set (Formula a) -> Bool
satisfiesAll ass = Set.foldl' (\t f -> t && satisfiable ass f) True

-- | Filters the formula that are satisfied by a valuation.
filterSatisfied :: (Ord a) => Map a Bool -> Set (Formula a) -> Set (Formula a)
filterSatisfied ass = Set.filter (satisfiable ass)

-- | Filters the formula that are not satisfied by a valuation.
filterUnsatisfied :: (Ord a) => Map a Bool -> Set (Formula a) -> Set (Formula a)
filterUnsatisfied ass = Set.filter (unsatisfiable ass)

-- | Number of satisfied formulas for a given valuation.
numSatisfied :: (Ord a) => Map a Bool -> Set (Formula a) -> Int
numSatisfied ass = Set.foldl' (\n f -> n + if satisfiable ass f then 1 else 0) 0

-- | All valuations that are true for a set of formulas.
trueValuations :: (Ord a) => Set (Formula a) -> [Map a Bool]
trueValuations fs = filter (`satisfiesAll` fs) (allAss fs)

-- | Builds a knowledge base from a list of strings. If the parser fails
-- to parse a formula, it is ignored.
fromStrings :: [String] -> KB (Predicate String)
fromStrings = foldr
  (\k acc ->
    case parseFOL k of
      Left _  -> acc
      Right f -> Set.insert f acc)
  Set.empty

-- | Entailment
--(|=)
