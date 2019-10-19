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
fmtMap = Map.foldrWithKey' (\k v acc -> T.concat [showTxt k, " -> ", showTxt v, "\n", acc]) ""

-- | Formats a map of sets to something
fmtMapOfSet :: (ShowTxt k, ShowTxt v, Ord k) => Map (Set k) v -> T.Text
fmtMapOfSet = Map.foldrWithKey' (\k v acc -> T.concat [fmtSet k, " -> ", showTxt v, "\n", acc]) ""

-- | Formats a map of sets (often used to represent undirected networks).
fmtMapSet :: (ShowTxt k0, ShowTxt k1, Ord k0, Ord k1) => Map k0 (Set k1) -> T.Text
fmtMapSet = Map.foldrWithKey' (\k v acc -> T.concat [showTxt k, " -> ", fmtSet v, "\n", acc]) ""

-- | Formats a map of maps (often used to represent networks.)
fmtMapMap :: (Show k0, Show k1, Show v, Ord k0, Ord k1) => Map k0 (Map k1 v) -> String
fmtMapMap = Map.foldrWithKey' vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges = Map.foldrWithKey' (\k v acc -> "(" ++ show k ++ ", " ++ show v ++ "), " ++ acc) ""
