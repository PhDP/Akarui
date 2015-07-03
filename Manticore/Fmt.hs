-- | Functions to print some common data stuctures in a specific format. This
-- is mostly convenient for playing with Manticore in the console.
module Manticore.Fmt where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Manticore.Text

-- | Formats a set in the standard format (not a 'fromList').
fmtSet :: (Show k, Ord k) => Set k -> String
fmtSet s = addBrackets $ drop 2 $ Set.foldl' (\acc k -> acc ++ ", " ++ show k) "" s

-- | Formats a map.
fmtMap :: (Show k, Show v, Ord k) => Map k v -> String
fmtMap = Map.foldWithKey (\k v acc -> show k ++ " -> " ++ show v ++ "\n" ++ acc) ""

-- | Formats a map of sets to something
fmtMapOfSet :: (Show k, Show v, Ord k) => Map (Set k) v -> String
fmtMapOfSet = Map.foldWithKey (\k v acc -> fmtSet k ++ " -> " ++ show v ++ "\n" ++ acc) ""

-- | Formats a map of sets (often used to represent undirected networks).
fmtMapSet :: (Show k0, Show k1, Ord k0, Ord k1) => Map k0 (Set k1) -> String
fmtMapSet = Map.foldWithKey (\k v acc -> show k ++ " -> " ++ fmtSet v ++ "\n" ++ acc) ""

-- | Formats a map of sets in GraphML format.
fmtGraphML :: (Show k0, Show k1, Ord k0, Ord k1) => String -> Map k0 (Set k1) -> String
fmtGraphML name m = hd ++ gr ++ nodes ++ allEdges ++ end
  where
    hd = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n" ++
          " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" ++
          " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
    gr = "  <graph id=\"" ++ name ++ "\" edgedefault=\"directed\">\n"
    nodes = Map.foldWithKey (\k _ acc -> "    <node id=\"" ++ show' k ++ "\"/>\n" ++ acc) "" m
    allEdges = Map.foldWithKey (\k v acc -> edges k v ++ acc) "" m
    edges from = Set.fold (\to acc -> "    <edge source=\"" ++ show' from ++ "\" target=\"" ++ show' to ++ "\"/>\n" ++ acc) ""
    end = "  </graph>\n</graphml>\n"
    -- To prevent issues with show String.
    show' s = rmQuotes $ show s

-- | Formats a map of maps (often used to represent networks.)
fmtMapMap :: (Show k0, Show k1, Show v, Ord k0, Ord k1) => Map k0 (Map k1 v) -> String
fmtMapMap = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges = Map.foldrWithKey (\k v acc -> "(" ++ show k ++ ", " ++ show v ++ "), " ++ acc) ""
