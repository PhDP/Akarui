{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Type and functions for fuzzy logic.
module Faun.FuzzyLogic
( FuzzyLogic
, resolve
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Faun.Symbols (setnotation)
import Faun.Formula
import qualified Faun.FuzzySet as FS
import Faun.BinT
import Faun.ShowTxt
import Faun.PrettyPrint

-- | A fuzzy logic formula.
type FuzzyLogic = Formula FS.FuzzySet

instance Show FuzzyLogic where
  show = T.unpack . prettyPrintFm setnotation

instance ShowTxt FuzzyLogic where
  showTxt = prettyPrintFm setnotation

instance PrettyPrint FuzzyLogic where
  prettyPrint = prettyPrintFm

-- | Resolve the fuzzy system.
resolve :: FuzzyLogic -> FS.FuzzySet
resolve f = let f' = coreOp f in
  case f' of
    Atom s          -> s
    Not x           -> FS.complement (resolve x)
    BinOp Or x y    -> FS.intersection (resolve x) (resolve y)
    BinOp And x y   -> FS.union (resolve x) (resolve y)
    BinOp{}         -> error "Only core operations (or, and) should be left."
    _               -> FS.FuzzySet Map.empty
