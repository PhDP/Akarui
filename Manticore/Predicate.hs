-- | Type and functions for predicates: the atoms of first-order logic.
module Manticore.Predicate where

import Manticore.Text
import Manticore.Term (Term)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Manticore.Term as Term

-- | Predicates are atoms (thus they evaluate to true/false) mapping a list
-- of terms (objects) to a truth value.
data Predicate t =
  -- | Builds a predicate with a string and a list of terms.
  Predicate String [Term t]

instance (Eq t) => Eq (Predicate t) where
  (Predicate n0 ts0) == (Predicate n1 ts1) =
    n0 == n1 && length ts0 == length ts1 && all (uncurry (==)) (zip ts0 ts1)

instance (Ord t) => Ord (Predicate t) where
  (Predicate n0 ts0) `compare` (Predicate n1 ts1) = Term.compareFun n0 ts0 n1 ts1

instance (Show t) => Show (Predicate t) where
  show = rmQuotes . showPredicate
    where
      showPredicate (Predicate n ts) =
        n ++ "(" ++ (if null ts then "" else terms) ++ ")"
        where terms = mkString $ map show ts

-- | Gathers the constants in a predicate.
constants :: (Ord a) => Predicate a -> Set a
constants (Predicate _ ts) = foldl' (\a t -> Set.union (Term.constants t) a) Set.empty ts

-- | Shows the internal structure of the predicate.
showStruct :: (Show a) => Predicate a -> String
showStruct (Predicate n ts) =
  "Predicate " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
  where terms = mkString (map Term.showStruct ts)

-- | Tests if the term is 'grounded', i.e. if it has no variables.
ground :: Predicate t -> Bool
ground (Predicate _ ts) = all Term.ground ts

-- | Tests if the predicate has a certain variable.
hasVar :: (Eq a) => a -> Predicate a -> Bool
hasVar v (Predicate _ ts) = any (Term.hasVar v) ts

-- | Replace a term with another.
substitute :: (Eq a) => Term a -> Term a -> Predicate a -> Predicate a
substitute t0 t1 (Predicate n ts) = Predicate n $ map (Term.substitute t0 t1) ts
