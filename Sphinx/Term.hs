module Sphinx.Term where

import Data.List (foldl')
import Sphinx.Text

-- | A term represents objects. It is either a
-- constants, which represent an object, a variable
-- that ranges over objects, or a function of an
-- arbitrary number of objects to an object.
data Term t =
    Variable t
  | Constant t
  | Function String [Term t]

instance (Eq t) => Eq (Term t) where
  (Variable t0) == (Variable t1) = t0 == t1
  (Constant t0) == (Constant t1) = t0 == t1
  (Function n0 ts0) == (Function n1 ts1) = n0 == n1 && all (uncurry (==)) (zip ts0 ts1)
  _ == _ = False

showTerm :: (Show t) => Term t -> String
showTerm t = case t of
  Variable x    -> show x
  Constant x    -> show x
  Function n ts -> n ++ "(" ++ (if null ts then "" else terms) ++ ")"
    where terms = mkString $ map showTerm ts

-- Returns the number of variables in the term.
numVars :: (Num n) => Term t -> n
numVars t = case t of
  Variable _    -> 1
  Constant _    -> 0
  Function _ ts -> foldl' (\acc trm -> acc + numVars trm) 0 ts

-- Returns the number of constants in the term.
numCons :: (Num n) => Term t -> n
numCons t = case t of
  Variable _    -> 0
  Constant _    -> 1
  Function _ ts -> foldl' (\acc trm -> acc + numCons trm) 0 ts

-- Returns the number of functions in the term.
numFuns :: (Num n) => Term t -> n
numFuns t = case t of
  Variable _    -> 0
  Constant _    -> 0
  Function _ ts -> 1 + foldl' (\acc trm -> acc + numFuns trm) 0 ts

-- Tests if the term is 'grounded', i.e. if it has no variables.
groundTerm :: Term t -> Bool
groundTerm (Variable _) = False
groundTerm (Constant _) = True
groundTerm (Function _ ts) = all groundTerm ts

-- Show the internal structure of the term.
showTermStruct :: (Show a) => Term a -> String
showTermStruct t = case t of
  Variable x    -> "Variable (" ++ show x ++ ")"
  Constant x    -> "Constant (" ++ show x ++ ")"
  Function n ts ->
    "Function " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
    where terms = mkString (map showTermStruct ts)
