-- | Module for discrete fuzzy sets.
module Akarui.MVL.FuzzySet
( FuzzySet(..)
 , fromSet
 , subsetOf
 , elementOf
 , size
 , cardinality
 , support
 , supportSize
 , singleton
 , union
 , intersection
 , complement
 , remove0s
) where

import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Akarui.ShowTxt
import Akarui.Utils
import Akarui.FOL.PrettyPrint
import Akarui.MVL.Fuzzy

-- | A discrete fuzzy set.
data FuzzySet a = MapFS (Map a Fuzzy)

instance (ShowTxt a) => Show (FuzzySet a) where
  show = T.unpack . showByElem

instance (ShowTxt a) => ShowTxt (FuzzySet a) where
  showTxt = showByElem

instance (ShowTxt a) => PrettyPrint (FuzzySet a) where
  prettyPrint _ = showByElem

elementOf :: (Ord a) => a -> FuzzySet a -> Fuzzy
elementOf e (MapFS m) = case Map.lookup e m of Just d -> d; _ -> mkFuzzy 0

-- | Number of elements in the fuzzyset.
size :: (Ord a) => FuzzySet a -> Int
size (MapFS m) = Map.size m

cardinality :: (Ord a) => FuzzySet a -> Double
cardinality (MapFS m) = Map.foldr (\f acc -> acc + toDouble f) 0 m

-- | Converts a normal set into a fuzzyset with degree 1.0 for all elements.
fromSet :: (Ord a) => Set a -> FuzzySet a
fromSet s = MapFS $ Set.foldr' (\k acc -> Map.insert k fuzzyTrue acc) Map.empty s

-- | A set f0 is a subset of f1 if, for all elements, degree(e0) < degree(e1).
subsetOf :: (Ord a) => FuzzySet a -> FuzzySet a -> Bool
subsetOf (MapFS m0) (MapFS m1) = allKeyVal sub m0
  where sub k v = case Map.lookup k m1 of Just v1 -> v < v1; Nothing -> False

-- | The set of members with a degree greater than 0.
support :: (Ord a) => FuzzySet a -> Set a
support (MapFS m) = Map.keysSet $ Map.filter nonZero m

-- | Number of elements with a degree greater than 0.
supportSize :: (Ord a) => FuzzySet a -> Int
supportSize = Set.size . support

-- | Removes elements with a degree of 0.   -- size $ removes0 === support
remove0s :: (Ord a) => FuzzySet a -> FuzzySet a
remove0s (MapFS m) = MapFS $ Map.filter isFalse m

-- | Tests whether the fuzzy set is a singleton (has a support of 1).
singleton :: (Ord a) => FuzzySet a -> Bool
singleton f = Set.size (support f) == 1

-- | Union of two fuzzy sets (taking the max value).
union :: (Ord a) => FuzzySet a -> FuzzySet a -> FuzzySet a
union (MapFS m0) (MapFS m1) = MapFS $ Map.unionWith max m0 m1

-- | Intersection of two fuzzy sets (taking the min value).
intersection :: (Ord a) => FuzzySet a -> FuzzySet a -> FuzzySet a
intersection (MapFS m0) (MapFS m1) = MapFS $ Map.intersectionWith min m0 m1

-- | The complement of a fuzzy set.
complement :: (Ord a) => FuzzySet a -> FuzzySet a
complement (MapFS m) = MapFS $ Map.map invNeg m

showByElem :: (ShowTxt a) => FuzzySet a -> T.Text
showByElem (MapFS m) = T.concat ["{", txt, "}"]
  where elems = Map.toList m
        txt = T.intercalate ", " $ map (\(k, v) -> T.concat [showTxt k, "/", T.pack $ show v]) elems
