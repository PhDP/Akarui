module Sphinx.Predicate where

import Sphinx.Text
import Sphinx.Term

-- Predicates are atoms (thus they evaluate to true/false).
data Predicate t = Predicate String [Term t]

instance (Show t) => Show (Predicate t) where
  show = showPredicate

instance (Eq t) => Eq (Predicate t) where
  (Predicate n0 ts0) == (Predicate n1 ts1) =
    n0 == n1 && all (uncurry (==)) (zip ts0 ts1)

instance (Ord t) => Ord (Predicate t) where
  -- Should compare arguments when n0 == n1
  (Predicate n0 _) `compare` (Predicate n1 _) = n0 `compare` n1

showPredicate :: (Show t) => Predicate t -> String
showPredicate (Predicate n ts) =
  n ++ "(" ++ (if null ts then "" else terms) ++ ")"
  where terms = mkString $ map showTerm ts

-- Show the internal structure of the predicate.
showPreStruct :: (Show a) => Predicate a -> String
showPreStruct (Predicate n ts) =
  "Predicate " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
  where terms = mkString (map showTermStruct ts)
