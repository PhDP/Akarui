-- | Type and functions for fuzzy logic.
module Faun.FuzzyLogic
( FuzzyLogic
, resolve
, fuzzyFm
) where

import qualified Data.Map as Map
import Faun.Formula
import qualified Faun.FuzzySet as FS
import Faun.BinT

-- | A first-order logic formula is simply a formula of predicates.
type FuzzyLogic = Formula FS.FuzzySet

fuzzyFm :: FuzzyLogic
fuzzyFm = BinOp And a (BinOp Or b c)
  where
    a = Not $ Atom $ FS.FuzzySet $ Map.fromList [("a", 0.5), ("b", 0.5), ("c", 0.8), ("e", 0.2), ("z", 1.0)]
    b = Atom $ FS.FuzzySet $ Map.fromList [("x", 0.5), ("a", 0.1), ("d", 0.8), ("y", 0.2), ("h", 0.4)]
    c = Atom $ FS.FuzzySet $ Map.fromList [("a", 0.1), ("d", 0.1), ("h", 0.8)]

-- | Resolve the fuzzy system.
resolve :: FuzzyLogic -> FS.FuzzySet
resolve f = let f' = coreOp f in
  case f' of
    Atom s          -> s
    Not x           -> FS.complement (resolve x)
    BinOp Or x y    -> FS.intersection (resolve x) (resolve y)
    BinOp And x y   -> FS.union (resolve x) (resolve y)
    BinOp _ _ _     -> error "Only core operations (or, and) should be left."
    _               -> FS.FuzzySet Map.empty

