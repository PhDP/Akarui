-- | Useful functions with no place to call home (can you hear the violins?).
module Faun.Utils where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Checks whether 'c' is between 'l' and 'r' (including those).
between :: (Ord a) => a -> a -> a -> Bool
between l c r = if l <= c then c <= r else False

-- | Max value in a map.
maxVal :: (Ord k, Ord v) => Map k v -> v
maxVal m = Map.foldr max f m
  where (_, f) = Map.findMax m

-- | Max value in a map.
minVal :: (Ord k, Ord v) => Map k v -> v
minVal m = Map.foldr min f m
  where (_, f) = Map.findMin m

-- | Converts a Map k v to a Map v (Set k)
reverseMap :: (Ord k, Ord v) => Map k v -> Map v (Set k)
reverseMap = Map.foldrWithKey (\k v acc -> Map.insert v (inSet k v acc) acc) Map.empty
  where
    -- If the key is absent, add a new set, otherwise insert in the set.
    inSet k v m = case Map.lookup v m of
      Just s -> Set.insert k s
      Nothing -> Set.fromList [k]

-- | 'any' with a Map's keys.
anyKey :: (Ord k) => (k -> Bool) -> Map k v -> Bool
anyKey p = Map.foldrWithKey (\key _ acc -> acc || p key) False

-- | 'all' with a Map's keys.
allKeys :: (Ord k) => (k -> Bool) -> Map k v -> Bool
allKeys p = Map.foldrWithKey (\key _ acc -> acc && p key) True

-- | 'any' with a Map's keys and values
anyKeyVal :: (Ord k) => (k -> v -> Bool) -> Map k v -> Bool
anyKeyVal p = Map.foldrWithKey (\key val acc -> acc || p key val) False

-- | 'all' with a Map's keys.
allKeyVal :: (Ord k) => (k -> v -> Bool) -> Map k v -> Bool
allKeyVal p = Map.foldrWithKey (\key val acc -> acc && p key val) True

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

-- | Surrounds a list.
surround :: a -> [a] -> [a]
surround e ls = (e : ls) ++ [e]

-- | Drops the last two items of a list.
dropLst2 :: [a] -> [a]
dropLst2 = init . init
