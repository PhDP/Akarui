-- | Type and functions for first-order predicate logic.
module Sphinx.FOL where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Sphinx.Formula
import Sphinx.Predicate
import Sphinx.Term

-- | A first-order logic formula is simply a formula of predicates.
type FOL t = Formula (Predicate t)

-- | Tests if the formula is 'grounded', i.e. if it has no variables.
groundFm :: FOL t -> Bool
groundFm f = case f of
  Atom (Predicate _ ts) -> all groundTerm ts
  BinOp _ x y           -> groundFm x || groundFm y
  Qualifier _ _ x       -> groundFm x
  _                     -> False

-- | Gathers all the variables in a first-order logic formula.
variables :: (Ord t) => FOL t -> Set t
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

-- | Returns true if the formula has functions. This is often used in algorithms
-- where we must ensure all functions have been resolved to an object.
hasFun :: FOL t -> Bool
hasFun f = case f of
  Atom (Predicate _ ts) -> any (\trm -> (numFuns trm :: Int) > 0) ts
  BinOp _ x y           -> hasFun x || hasFun y
  Qualifier _ _ x       -> hasFun x
  _                     -> False

-- | Shows the internal structure of the first-order logic formula. This is
-- mostly useful for testing and making sure the formula has the correct
-- structure.
showFOLStruct :: (Show a) => FOL a -> String
showFOLStruct f = case f of
  Atom a          -> showPredStruct a
  Top             -> "Top"
  Bottom          -> "Bottom"
  Not x           -> "Not (" ++ showFOLStruct x ++ ")"
  BinOp b x y     -> show b ++ " (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  Qualifier q v x -> show q ++ " " ++ v ++ "(" ++ showFOLStruct x ++ ")"
