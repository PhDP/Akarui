module Sphinx.FOL where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Sphinx.Formula
import Sphinx.Symbols
import Sphinx.Text

data TypedObj = TypedObj
  { objName :: String
  , objType :: String}

instance Show TypedObj where
  show t = objName t ++ " -> " ++ objType t

-- | A term represents objects. It is either a
-- constants, which represent an object, a variable
-- that ranges over objects, or a function of an
-- arbitrary number of objects to an object.
data Term t =
    Variable t
  | Constant t
  | Function String [Term t]

instance (Show t) => Show (Term t) where
  show = showTerm symbolic

showTerm :: (Show t) => Symbols -> Term t -> String
showTerm s t = case t of
  Variable x    -> show x
  Constant x    -> show x
  Function n ts -> n ++ "(" ++ (if null ts then "" else terms) ++ ")"
    where terms = mkString $ map (showTerm s) ts

-- Predicates are atoms (thus they evaluate to true/false).
data Predicate t = Predicate String [Term t]

instance (Show t) => Show (Predicate t) where
  show = showPredicate symbolic

showPredicate :: (Show t) => Symbols -> Predicate t -> String
showPredicate s (Predicate n ts) =
  n ++ "(" ++ (if null ts then "" else terms) ++ ")"
  where terms = mkString $ map (showTerm s) ts

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

-- Tests if the formula is 'grounded', i.e. if it has no variables.
groundFm :: Formula (Predicate t) -> Bool
groundFm f = case f of
  Atom (Predicate _ ts) -> all groundTerm ts
  BinOp _ x y           -> groundFm x || groundFm y
  Qualifier _ _ x       -> groundFm x
  _                     -> False

-- Gathers all the variables in a first-order logic formula.
variables :: (Ord t) => Formula (Predicate t) -> Set t
variables = gat Set.empty
  where
    -- Gathers variables from terms
    gatT s term = case term of
      Function _ ts -> foldl' gatT Set.empty ts
      Variable v    -> Set.insert v s
      Constant _    -> Set.empty
    -- Gathers variables from formula
    gat s fm = case fm of
      Atom (Predicate _ ts) -> foldl' gatT Set.empty ts
      Not x                 -> Set.union (gatE x) s
      BinOp _ x y           -> Set.unions [gatE x, gatE y, s]
      Qualifier _ _ x       -> Set.union (gatE x) s
      _                     -> Set.empty
    -- Gathers with an empty set
    gatE = gat Set.empty

-- Returns true if the formula has functions.
hasFun :: Formula (Predicate t) -> Bool
hasFun f = case f of
  Atom (Predicate _ ts) -> any (\trm -> (numFuns trm :: Int) > 0) ts
  BinOp _ x y           -> hasFun x || hasFun y
  Qualifier _ _ x       -> hasFun x
  _                     -> False

-- Show the internal structure of the predicate.
showPreStruct :: (Show a) => Predicate a -> String
showPreStruct (Predicate n ts) =
  "Predicate " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
  where terms = mkString (map showTermStruct ts)

-- Show the internal structure of the term.
showTermStruct :: (Show a) => Term a -> String
showTermStruct t = case t of
  Variable x    -> "Variable (" ++ show x ++ ")"
  Constant x    -> "Constant (" ++ show x ++ ")"
  Function n ts ->
    "Function " ++ n ++ " [" ++ (if null ts then "" else terms) ++ "]"
    where terms = mkString (map showTermStruct ts)

-- Show the internal structure of the first-order logic formula.
showFOLStruct :: (Show a) => Formula (Predicate a) -> String
showFOLStruct f = case f of
  Atom a                -> showPreStruct a
  Top                   -> "Top"
  Bottom                -> "Bottom"
  Not x                 -> "Not (" ++ showFOLStruct x ++ ")"
  BinOp And x y         -> "And (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  BinOp Or x y          -> "Or (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  BinOp Implies x y     -> "Implies (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  BinOp Xor x y         -> "Xor (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  BinOp Iff x y         -> "Iff (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  Qualifier ForAll v x  -> "ForAll " ++ v ++ "(" ++ showFOLStruct x ++ ")"
  Qualifier Exists v x  -> "Exists " ++ v ++ "(" ++ showFOLStruct x ++ ")"
