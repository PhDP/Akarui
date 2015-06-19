-- | Type and functions for predicates: the atoms of first-order logic.
module Sphinx.Predicate where

import Sphinx.Text
import Sphinx.Term

-- | Predicates are atoms (thus they evaluate to true/false) mapping a list
-- of terms (objects) to a truth value.
data Predicate t =
  -- | Builds a predicate with a string and a list of terms.
  Predicate String [Term t]

instance (Eq t) => Eq (Predicate t) where
  (Predicate n0 ts0) == (Predicate n1 ts1) =
    n0 == n1 && all (uncurry (==)) (zip ts0 ts1)

instance (Ord t) => Ord (Predicate t) where
  (Predicate n0 ts0) `compare` (Predicate n1 ts1) = compareFun n0 ts0 n1 ts1

instance (Show t) => Show (Predicate t) where
  show = rmQuotes . showPredicate
    where
      showPredicate (Predicate n ts) =
        n ++ "(" ++ (if null ts then "" else terms) ++ ")"
        where terms = mkString $ map show ts

-- | Shows the internal structure of the predicate.
showPreStruct :: (Show a) => Predicate a -> String
showPreStruct (Predicate n ts) =
  "Predicate " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
  where terms = mkString (map showTermStruct ts)
