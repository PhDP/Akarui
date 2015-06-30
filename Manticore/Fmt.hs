-- | Functions to print some common data stuctures in a specific format. This
-- is mostly convenient for playing with Manticore in the console.
module Manticore.Fmt where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Manticore.Text

-- | Formats a map of sets (often used to represent undirected networks).
fmtMapSet :: (Show k0, Show k1, Ord k0, Ord k1) => Map k0 (Set k1) -> String
fmtMapSet = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges e = dropLst2 $ Set.foldr (\k acc -> show k ++ ", " ++ acc) "" e

-- | Formats a map of maps (often used to represent networks.)
fmtMapMap :: (Show k0, Show k1, Show v, Ord k0, Ord k1) => Map k0 (Map k1 v) -> String
fmtMapMap = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges = Map.foldrWithKey (\k v acc -> "(" ++ show k ++ ", " ++ show v ++ "), " ++ acc) ""
