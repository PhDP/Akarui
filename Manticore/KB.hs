{-# LANGUAGE OverloadedStrings #-}

-- | A knowledge base is a set of formulas. See MLN.fs for probabilistic
-- knowledge bases.
module Manticore.KB where

import qualified Data.Set as Set
import Data.Set (Set)
import Manticore.Formula

-- | A knowledge base is a set of formula.
type KB a = Set (Formula a)

-- | Pretty print a knowledge base.
showKB :: (Show a) => KB a -> String
showKB = Set.foldr' (\k acc -> show k ++ "\n" ++ acc) ""

-- | Gathers all atoms in the knowledge base.
allAtoms :: (Ord a) => KB a -> Set a
allAtoms = Set.foldl' (\acc k -> Set.union acc (atoms k)) Set.empty
