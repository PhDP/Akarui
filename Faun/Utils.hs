-- | Useful functions with no place to call home (can you hear the violins?).
module Faun.Utils where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Builds a set of tuple from a map.
mapToSet :: (Ord k, Ord v) => Map k v -> Set (k, v)
mapToSet = Map.foldrWithKey (\k v acc -> Set.insert (k, v) acc) Set.empty

-- | Strict left set fold without an initial value (it's the min of the set).
sfoldl1' :: (Ord a) => (a -> a -> a) -> Set a -> a
sfoldl1' f s = Set.foldl' f first rest
  where
    first = Set.findMin s
    rest = Set.delete first s

-- | Strict right set fold without an initial value (it's the max of the set).
sfoldr1' :: (Ord a) => (a -> a -> a) -> Set a -> a
sfoldr1' f s = Set.foldr' f first rest
  where
    first = Set.findMax s
    rest = Set.delete first s
