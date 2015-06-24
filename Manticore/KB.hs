{-# LANGUAGE OverloadedStrings #-}

-- | A knowledge base is a set of formulas. See MLN.hs for probabilistic
-- knowledge bases.
module Manticore.KB where

import qualified Data.Set as Set
import Data.Set (Set)
import Manticore.Formula
import Manticore.Parser
import Manticore.Predicate

-- | A knowledge base is a set of formula.
type KB a = Set (Formula a)

-- | Pretty print a knowledge base.
showKB :: (Show a) => KB a -> String
showKB = Set.foldr' (\k acc -> show k ++ "\n" ++ acc) ""

-- | Gathers all atoms in the knowledge base.
allAtoms :: (Ord a) => KB a -> Set a
allAtoms = Set.foldl' (\acc k -> Set.union acc (atoms k)) Set.empty

-- | Builds a knowledge base from a list of strings. If the parser fails
-- to parse a formula, it is ignored.
fromStrings :: [String] -> KB (Predicate String)
fromStrings = foldr
  (\k acc ->
    case (parseFOL k) of
      Left _  -> acc
      Right f -> Set.insert f acc)
  Set.empty

