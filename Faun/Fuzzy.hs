-- | Faun.Fuzzy is a fun functional set of functions for fuzzy logic
module Faun.Fuzzy where

import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate)
import Faun.Utils

newtype FuzzySet = FuzzySet (Map String Double)
    deriving (Eq)

instance Show (FuzzySet) where
  show (FuzzySet m) = "{" ++ str ++ "}"
    where elems = Map.toList m
          str = intercalate ", " $ map (\(k, v) -> k ++ "/" ++ show v) elems

-- | ...
showByDegree :: FuzzySet -> String
showByDegree (FuzzySet m) = "{" ++ str ++ "}"
  where elems = Map.toList $ reverseMap m
        str = intercalate ", " $ map showSet elems
        showSet (k, s) = intercalate ", " $ Set.toList $ Set.map (\v -> show k ++ "/" ++ v) s

-- Standard functions on sets

-- | ...
subsetOf :: FuzzySet -> FuzzySet -> Bool
subsetOf (FuzzySet m0) (FuzzySet m1) = allKeyVal sub m0
  where sub k v = case Map.lookup k m1 of Just v1 -> v < v1; Nothing -> False

-- | This one is tricky: in fuzzy set F, e ∈ F iff e has a degree of 1.0 in F.
elementOf :: String -> FuzzySet -> Bool
elementOf e (FuzzySet m) = case Map.lookup e m of Just 1.0 -> True; Nothing -> False

-- | The set of members with a degree greater than 0.
support :: FuzzySet -> Set String
support (FuzzySet m) = Map.keysSet $ Map.filter (> 0.0) m

-- | Tests whether the fuzzy set is a singleton (has a support of 1).
singleton :: FuzzySet -> Bool
singleton f = Set.size (support f) == 1

union :: FuzzySet -> FuzzySet -> FuzzySet
union (FuzzySet m0) (FuzzySet m1) = FuzzySet $ Map.unionWith max m0 m1

intersection :: FuzzySet -> FuzzySet -> FuzzySet
intersection (FuzzySet m0) (FuzzySet m1) = FuzzySet $ Map.unionWith min m0 m1

complement :: FuzzySet -> FuzzySet
complement (FuzzySet m) = FuzzySet $ Map.map (1.0 -) m

-- Operations unique to fuzzy sets

concentration :: FuzzySet -> FuzzySet
concentration (FuzzySet m) = FuzzySet $ Map.map (**2) m

dilation :: FuzzySet -> FuzzySet
dilation (FuzzySet m) = FuzzySet $ Map.map sqrt m

normalization :: FuzzySet -> FuzzySet
normalization (FuzzySet m) = FuzzySet $ Map.map (/ maxv) m
  where maxv = maxVal m
