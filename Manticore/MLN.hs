{-# LANGUAGE OverloadedStrings #-}

-- | Types and algorithms for Markov logic networks.
module Manticore.MLN where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Manticore.FOL
import Manticore.Formula
import Manticore.Predicate
import Manticore.Term
import Manticore.Parser
import Manticore.Symbols
import qualified Manticore.KB as KB

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.
type MLN t = Map (FOL t) Double

-- | Print MLN.
showMLN :: (Show t) => Symbols -> MLN t -> String
showMLN s = Map.foldrWithKey (\k v acc -> showWFormula s k v ++ "\n" ++ acc) ""

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
allPredicates :: (Ord t) => MLN t -> Set (Predicate t)
allPredicates = Map.foldWithKey (\k _ acc -> Set.union (atoms k) acc) Set.empty

-- | Build ground network for Markov logic.
buildGroundNetwork :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> Map (Predicate String) (Set (Predicate String))
buildGroundNetwork m ts mln = Set.foldr' (\p acc -> Map.insert p (neighbours p) acc) Map.empty ps
  where
    gs = Set.foldr' (\g acc -> Set.union (groundings m ts g) acc) Set.empty (Map.keysSet mln)
    ps = KB.allPredicates gs
    neighbours p = Set.delete p $ KB.allPredicates $ Set.filter (hasPred p) gs

-- | Builds a weighted knowledge base from a list of strings. If the parser
-- fails to parse a formula, it is ignored.
fromStrings :: [String] -> MLN String
fromStrings = foldr
  (\k acc ->
    case parseWFOL k of
      Left _        -> acc
      Right (f, w)  -> Map.insert f w acc)
  Map.empty
