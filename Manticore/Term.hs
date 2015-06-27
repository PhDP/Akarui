-- | Type and functions for terms: the objects of first-order logic.
module Manticore.Term where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import Data.Monoid ((<>), mconcat)
import Data.Maybe (fromMaybe)
import Manticore.Text

-- | A term represents an object. Terms are not atoms, they are found in
-- predicates in first-order logic.
--
-- Warning: for Term String, several algorithms assume the string of variables
-- start with a lowercase character, while constants start with an uppercase
-- character. For example, the parser uses the case of the first character to
-- distinguish variables from constants.
data Term t =
  -- | Variables range over objects. For example the variable x might be a
  -- number, t could be a city, etc.
    Variable t
  -- | Constants represent actual objects: the number 0, Kyoto, Quebec City,
  -- Aristotle could all be constants.
  | Constant t
  -- | Functions map objects to objects. The function 'Add' maps numbers to
  -- a number, the function "CapitalOf" maps a city to a country, etc.
  | Function String [Term t]

instance (Eq t) => Eq (Term t) where
  (Variable t0) == (Variable t1) = t0 == t1
  (Constant t0) == (Constant t1) = t0 == t1
  (Function n0 ts0) ==
    (Function n1 ts1) = n0 == n1 && all (uncurry (==)) (zip ts0 ts1)
  _ == _ = False

instance (Ord t) => Ord (Term t) where
  (Variable t0) `compare` (Variable t1) = t0 `compare` t1
  (Variable t0) `compare` (Constant t1) = t0 `compare` t1
  (Variable _) `compare` (Function _ _) = LT
  (Constant t0) `compare` (Constant t1) = t0 `compare` t1
  (Constant t0) `compare` (Variable t1) = t0 `compare` t1
  (Constant _) `compare` (Function _ _) = LT
  (Function n0 ts0) `compare` (Function n1 ts1) = compareFun n0 ts0 n1 ts1
  (Function _ _) `compare` _ = GT

instance (Show t) => Show (Term t) where
  show = rmQuotes . showTerm
    where
      showTerm t = case t of
        Variable x    -> show x
        Constant x    -> show x
        Function n ts -> n ++ "(" ++ (if null ts then "" else terms) ++ ")"
          where terms = mkString $ map showTerm ts

-- | Used to compare names and arguments for functions and predicate. First
-- look at the name, then the number of arguments, and finally for functions
-- with the same name and argument, look at the first term that differ.
compareFun :: (Ord t) => String -> [Term t] -> String -> [Term t] -> Ordering
compareFun n0 ts0 n1 ts1 =
     (n0 `compare` n1)
  <> (length ts0 `compare` length ts1)
  <> mconcat (zipWith compare ts0 ts1)

-- | Returns the number of variables in the term.
numVars :: (Num n) => Term t -> n
numVars t = case t of
  Variable _    -> 1
  Constant _    -> 0
  Function _ ts -> foldl' (\acc trm -> acc + numVars trm) 0 ts

-- | Returns the number of constants in the term.
numCons :: (Num n) => Term t -> n
numCons t = case t of
  Variable _    -> 0
  Constant _    -> 1
  Function _ ts -> foldl' (\acc trm -> acc + numCons trm) 0 ts

-- | Returns the number of functions in the term.
numFuns :: (Num n) => Term t -> n
numFuns t = case t of
  Variable _    -> 0
  Constant _    -> 0
  Function _ ts -> 1 + foldl' (\acc trm -> acc + numFuns trm) 0 ts

-- | Substitute a term for another.
substitute :: (Eq t) => Term t -> Term t -> Term t -> Term t
substitute old new (Function n ts) =
  if old == Function n ts then new
  else Function n $ map (substitute old new) ts
substitute old new t0 = if t0 == old then new else t0

-- | Shows the internal structure of the term. This is particularly useful
-- to distinguish variables from constants in Term String, where otherwise
-- it would be impossible to tell them apart.
showStruct :: (Show a) => Term a -> String
showStruct t = case t of
  Variable x    -> "Variable (" ++ show x ++ ")"
  Constant x    -> "Constant (" ++ show x ++ ")"
  Function n ts ->
    "Function " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
    where terms = mkString (map showStruct ts)

-- Unify these functions under some type class for FOL, Predicate, and Term?

-- | Tests if the term is 'grounded', i.e. if it has no variables.
ground :: Term t -> Bool
ground t = case t of
  Variable _    -> False
  Constant _    -> True
  Function _ ts -> all ground ts

-- | Tests if the term has a specific variable.
hasVar :: (Eq a) => a -> Term a -> Bool
hasVar v t = case t of
  Variable x    -> v == x
  Constant _    -> False
  Function _ ts -> any (hasVar v) ts

-- | Resolve functions with a map from function names & args to a term.
resolveFun :: (Ord a) => Map (String, [Term a]) (Term a) -> Term a -> Term a
resolveFun m t = case t of
  Function n ts -> fromMaybe t $ Map.lookup (n, map (resolveFun m) ts) m
  _             -> t
