-- | Type and functions for first-order predicate logic.
module Manticore.FOL where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Manticore.Formula
import Manticore.Predicate
import Manticore.Term

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

-- | Substitute a term in the formula.
substitute :: (Eq a) => Term a -> Term a -> FOL a -> FOL a
substitute old new f = case f of
  Atom (Predicate n ts)
    -> Atom $ Predicate n $ map (subTerm old new) ts
  Not x           -> Not $ substitute old new x
  BinOp b x y     -> BinOp b (substitute old new x) (substitute old new y)
  Qualifier q v x -> Qualifier q v (substitute old new x)
  _               -> f -- Top / Bottom

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

-- | Resolve functions by providing a map from function name & arguments to terms.
resolveFun :: (Ord a) => Map (String, [Term a]) (Term a) -> FOL a -> FOL a
resolveFun m f = case f of
  Atom (Predicate n ts) ->
    Atom $ Predicate n $ map (termResolveFun m) ts
  Not x             -> Not (resolveFun m x)
  BinOp b x y       -> BinOp b (resolveFun m x) (resolveFun m y)
  Qualifier q v x   -> Qualifier q v (resolveFun m x)
  _                 -> f

-- | Resolve predicates by providing a map from name & arguments to bool.
resolvePre :: (Ord a) => Map (String, [Term a]) Bool -> FOL a -> FOL a
resolvePre m f = case f of
  Atom (Predicate n ts) -> case Map.lookup (n, ts) m of
    Just True   -> Top
    Just False  -> Bottom
    Nothing     -> f
  Not x             -> Not (resolvePre m x)
  BinOp b x y       -> BinOp b (resolvePre m x) (resolvePre m y)
  Qualifier q v x   -> Qualifier q v (resolvePre m x)
  _                 -> f

-- | Resolves universal qualifiers, substituting the variables in the 'ForAll'
-- for a given term (a constant, generally).
resolveForAll :: String -> Term String -> FOL String -> FOL String
resolveForAll v t f = case f of
  Not x                 -> Not $ resolveForAll v t x
  BinOp b x y           -> BinOp b (resolveForAll v t x) (resolveForAll v t y)
  Qualifier ForAll v' x  ->
    if v == v' then substitute (Variable v) t x
    else Qualifier ForAll v' (resolveForAll v t x)
  Qualifier Exists v' x  -> Qualifier Exists v' (resolveForAll v t x)
  _                     -> f

-- | Takes a formula, a map between functions and constants, and a list of
-- constants to produce a set of groundings.
groundings :: FOL String -> Map (String, [Term String]) (Term String) -> [Term String]-> Set (FOL String)
groundings f m cs = gs
  where
    groundSub v f' = case f' of
      Atom p ->
        if predHasVar v p then
          let as = map (\c -> Atom $ subsPre (Variable v) c p) cs in
          foldr1 (BinOp Or) as
        else
          f'
      Not x            -> Not $ groundSub v x
      BinOp b x y      -> BinOp b (groundSub v x) (groundSub v y)
      Qualifier q v' x -> Qualifier q v' (groundSub v' x)
      _                -> f'

    existsVar f' = case f' of
      Not x                 -> Not $ existsVar x
      BinOp b x y           -> BinOp b (existsVar x) (existsVar y)
      Qualifier Exists v x  -> existsVar $ groundSub v x
      Qualifier ForAll v x  -> Qualifier ForAll v $ existsVar x
      _                     -> f'

    f0 = existsVar f
    g0 = Set.fromList [f0]
    vs = uniQualVars f0
    gs = Set.foldr'
      (\v g -> Set.foldr'
        (\f'' acc ->
          Set.union (Set.delete f'' acc)
          (Set.fromList
            (map (\c -> simplify $ resolveFun m $ resolveForAll v c f) cs)))
        Set.empty g)
      g0 vs
