module Manticore.Formula where

import qualified Data.Set as Set
import Data.Set(Set)
import Manticore.Symbols

-- Supported binary connectives (in order of precedence).
data BinT = And | Or | Implies | Xor | Iff

-- Supported qualifiers.
data QualT = ForAll | Exists

-- A formula.
data Formula a =
    Top -- Another name for True to avoid confusion with Prelude.True.
  | Bottom -- Another name for 'False'.
  | Atom a
  | Not (Formula a)
  | BinOp BinT (Formula a) (Formula a)
  | Qualifier QualT String (Formula a)

instance Show a => Show (Formula a) where
  show = showFm symbolic

showFm :: (Show a) => Symbols -> Formula a -> String
showFm s = buildStr (0 :: Int)
  where
    --notSpace = " "
    --qualSpace = " "

    -- Surrouds the strings if b is true:
    surr b str = if b then "(" ++ str ++ ")" else str

    -- Format prefixes:
    showPrefix b pr sym p = surr b (sym ++ buildStr (pr + 1) p)

    -- Format infix operators:
    showInfix b pr sym p q =
      surr b (buildStr (pr + 1) p ++ " " ++ sym ++ " " ++ buildStr pr q)

    -- Recursive function to build the string:
    buildStr pr fm = case fm of
      Atom a                -> show a
      Top                   -> symTop s
      Bottom                -> symBottom s
      Not x                 -> showPrefix (pr > 12) 11 (symNot s) x
      BinOp And x y         -> showInfix (pr > 10) 10 (symAnd s) x y
      BinOp Or x y          -> showInfix (pr > 8) 8 (symOr s) x y
      BinOp Implies x y     -> showInfix (pr > 6) 6 (symImplies s) x y
      BinOp Xor x y         -> showInfix (pr > 4) 4 (symXor s) x y
      BinOp Iff x y         -> showInfix (pr > 2) 2 (symIff s) x y
      Qualifier ForAll v x  -> symForall s ++ " " ++ v ++ ", " ++ buildStr pr x
      Qualifier Exists v x  -> symExists s ++ " " ++ v ++ ", " ++ buildStr pr x

showFmStruct :: (Show a) => Formula a -> String
showFmStruct f = case f of
  Atom a                -> "Atom(" ++ show a ++ ")"
  Top                   -> "Top"
  Bottom                -> "Bottom"
  Not x                 -> "Not (" ++ showFmStruct x ++ ")"
  BinOp And x y         -> "And (" ++ showFmStruct x ++ ") (" ++ showFmStruct y ++ ")"
  BinOp Or x y          -> "Or (" ++ showFmStruct x ++ ") (" ++ showFmStruct y ++ ")"
  BinOp Implies x y     -> "Implies (" ++ showFmStruct x ++ ") (" ++ showFmStruct y ++ ")"
  BinOp Xor x y         -> "Xor (" ++ showFmStruct x ++ ") (" ++ showFmStruct y ++ ")"
  BinOp Iff x y         -> "Iff (" ++ showFmStruct x ++ ") (" ++ showFmStruct y ++ ")"
  Qualifier ForAll v x  -> "ForAll " ++ v ++ "(" ++ showFmStruct x ++ ")"
  Qualifier Exists v x  -> "Exists " ++ v ++ "(" ++ showFmStruct x ++ ")"

-- Gathers all atoms in the formula.
atoms :: (Ord a) => Formula a -> Set a
atoms f = gat f Set.empty
  where
    gat fm s = case fm of
      Atom z          -> Set.insert z s
      Not x           -> Set.union (gat x Set.empty) s
      BinOp _ x y     -> Set.unions [gat x Set.empty, gat y Set.empty, s]
      Qualifier _ _ x -> Set.union (gat x Set.empty) s
      _               -> Set.empty
