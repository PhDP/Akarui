{-# LANGUAGE OverloadedStrings #-}

-- | Types and algorithms for Markov logic networks.
module Manticore.MLN where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Manticore.FOL
import Manticore.Formula
import Manticore.Parser
import Manticore.Predicate
import Manticore.Symbols

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.
type MLN t = Map (FOL t) Double

-- | Print MLN.
showMLN :: (Show t) => Symbols -> MLN t -> String
showMLN s = Map.foldWithKey (\k v acc -> acc ++ "\n" ++ showWFormula s k v) ""

-- | Helper to print weighted formulas.
showWFormula :: (Show t) => Symbols -> FOL t -> Double -> String
showWFormula s f w = showW ++ replicate nSpaces ' ' ++ prettyPrintFm s f
  where
    showW = show w
    nSpaces = 24 - length showW

-- | Adds a formula to the markov logic network with a string (and the parser).
tellS :: String -> Double -> MLN String -> MLN String
tellS s w mln = case parseFOL s of
  Left _  -> mln
  Right f -> Map.insert f w mln

-- | Gathers all the predicates of a markov logic network in a set.
predicates :: (Ord t) => MLN t -> Set (Predicate t)
predicates = Map.foldWithKey (\k _ acc -> Set.union (atoms k) acc) Set.empty
