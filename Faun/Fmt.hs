-- | Functions to print some common data stuctures in a specific format. This
-- is mostly convenient for playing with Sphinx in the console.
module Faun.Fmt where

import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Faun.Text
import Faun.ShowTxt

-- | Formats a set in the standard format (not a 'fromList').
fmtSet :: (ShowTxt k, Ord k) => Set k -> T.Text
fmtSet s = addBrackets $ T.drop 2 $ Set.foldl' (\acc k -> T.concat [acc, ", ", showTxt k]) "" s

-- | Formats a map.
fmtMap :: (ShowTxt k, ShowTxt v, Ord k) => Map k v -> T.Text
fmtMap = Map.foldWithKey (\k v acc -> T.concat [showTxt k, " -> ", showTxt v, "\n", acc]) ""

-- | Formats a map of sets to something
fmtMapOfSet :: (ShowTxt k, ShowTxt v, Ord k) => Map (Set k) v -> T.Text
fmtMapOfSet = Map.foldWithKey (\k v acc -> T.concat [fmtSet k, " -> ", showTxt v, "\n", acc]) ""

-- | Formats a map of sets (often used to represent undirected networks).
fmtMapSet :: (ShowTxt k0, ShowTxt k1, Ord k0, Ord k1) => Map k0 (Set k1) -> T.Text
fmtMapSet = Map.foldWithKey (\k v acc -> T.concat [showTxt k, " -> ", fmtSet v, "\n", acc]) ""

-- | Formats a map of sets in GraphML format.
fmtGraphML :: (ShowTxt k0, ShowTxt k1, Ord k0, Ord k1) => T.Text -> Map k0 (Set k1) -> T.Text
fmtGraphML name m = T.concat [hd, gr, nodes, allEdges, end]
  where
    hd = T.concat
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          , "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
          , " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
          , " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"]
    gr = T.concat ["  <graph id=\"", name, "\" edgedefault=\"directed\">\n"]
    nodes = Map.foldWithKey (\k _ acc -> T.concat ["    <node id=\"", show' k, "\"/>\n", acc]) "" m
    allEdges = Map.foldWithKey (\k v acc -> T.concat [edges k v, acc]) "" m
    edges from = Set.fold (\to acc -> T.concat ["    <edge source=\"", show' from, "\" target=\"", show' to, "\"/>\n", acc]) ""
    end = "  </graph>\n</graphml>\n"
    -- To prevent issues with show String.
    show' s = rmQuotes $ showTxt s

-- | Formats a map of maps (often used to represent networks.)
fmtMapMap :: (Show k0, Show k1, Show v, Ord k0, Ord k1) => Map k0 (Map k1 v) -> String
fmtMapMap = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges = Map.foldrWithKey (\k v acc -> "(" ++ show k ++ ", " ++ show v ++ "), " ++ acc) ""
