{-# LANGUAGE OverloadedStrings #-}

-- | Types and algorithms for Markov logic networks. The module has quite a
-- few 'fromStrings' methods that take strings and parse them into data
-- structure to make it easier to play with Markov logic in the repl.
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
import Manticore.Network
import qualified Manticore.KB as KB
import Manticore.KB (KB)

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.
type MLN t = Map (FOL t) Double

-- | Prints a Markov logic network.
showMLN :: (Show t) => Symbols -> MLN t -> String
showMLN s = Map.foldrWithKey (\k v acc -> showWFormula s k v ++ "\n" ++ acc) ""

-- | Prints a weighted formula.
showWFormula :: (Show t) => Symbols -> FOL t -> Double -> String
showWFormula s f w = showW ++ replicate nSpaces ' ' ++ prettyPrintFm s f
  where
    showW = show w
    nSpaces = 24 - length showW

-- | Adds a formula to the markov logic network using the parser. If the parser
-- fails, the function returns the MLN unmodified.
tellS :: String -> Double -> MLN String -> MLN String
tellS s w mln = case parseFOL s of
  Left _  -> mln
  Right f -> Map.insert f w mln

-- | Gathers all the predicates of a markov logic network in a set.
allPredicates :: (Ord t) => MLN t -> Set (Predicate t)
allPredicates = Map.foldWithKey (\k _ acc -> Set.union (atoms k) acc) Set.empty

-- | Get all groundings from a Markov logic network.
allGroundings :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> KB (Predicate String)
allGroundings m ts mln = KB.allGroundings m ts (toKB mln)

-- | Builds a ground network for Markov logic.
groundNetwork :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> UNetwork (Predicate String)
groundNetwork m ts mln = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (groundings m ts g) acc) Set.empty (Map.keysSet mln)
    -- All the predicates:
    ps = KB.allPredicates gs
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ KB.allPredicates $ Set.filter (hasPred p) gs

-- | Algorithm to construct a network for Markov logic network inference.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 26.
constructNetwork :: Set (Predicate String) -> [Predicate String] -> [Term String] -> MLN String -> UNetwork (Predicate String)
constructNetwork query evidence ts mln = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (groundings Map.empty ts g) acc) Set.empty (Map.keysSet mln)
    -- Predicates in the network
    ps = step query query
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ KB.allPredicates $ Set.filter (hasPred p) gs
    -- One step of the algorithm
    step f g
      | Set.null f = g
      | Set.findMin f `elem` evidence = step (Set.deleteMin f) g
      | otherwise =
        let mbq = mb $ Set.findMin f in
        step
          (Set.union (Set.deleteMin f) (Set.intersection mbq g))
          (Set.union g mbq)

constructNetworkFromStrings :: String -> [String] -> [String] -> UNetwork (Predicate String)
constructNetworkFromStrings query ts mln = constructNetwork q e t m
  where
    (q, e) = case parseCondQuery query of
      Left _ -> (Set.empty, [])
      Right (q', e') -> (Set.fromList $ toPredicates $ Set.toList $ Map.keysSet q', toPredicates $ Set.toList $ Map.keysSet e')
    t = map Constant ts
    m = fromStrings mln

-- | Builds a weighted knowledge base from a list of strings. If the parser
-- fails to parse a formula, it is ignored.
fromStrings :: [String] -> MLN String
fromStrings = foldr
  (\k acc ->
    case parseWFOL k of
      Left _        -> acc
      Right (f, w)  -> Map.insert f w acc)
  Map.empty

-- | Converts a markov logic network to an unweighted knowledge base.
toKB :: (Ord t) => MLN t -> KB (Predicate t)
toKB = Map.keysSet

-- | Builds a Markov logic network with a first-order logic knowledge base
-- and a function mapping formulas to weights.
fromKB :: (Ord t) => (Formula (Predicate t) -> Double) -> KB (Predicate t) -> MLN t
fromKB w = Set.foldl' (\acc x -> Map.insert x (w x) acc) Map.empty
