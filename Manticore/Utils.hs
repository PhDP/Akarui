-- | Useful functions with no place to call home (can you hear the violins?).
module Manticore.Utils where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Builds a set of tuple from a map.
mapToSet :: (Ord k, Ord v) => Map k v -> Set (k, v)
mapToSet = Map.foldrWithKey (\k v acc -> Set.insert (k, v) acc) Set.empty
