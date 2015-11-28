{-# LANGUAGE FlexibleInstances #-}

module FOLSpec where

import Test.QuickCheck
import Control.Monad
import Faun.Formula
import Faun.FOL
import Faun.Parser
import Faun.Symbols
import PredicateSpec

genAtom :: Gen (FOL String)
genAtom = do
  p <- genPredicate
  return $ Atom p

-- Missing: existential and universal qualifiers:
instance Arbitrary (FOL String) where
  arbitrary = sized fol'
    where
      fol' 0 = elements [Top, Bottom]
      fol' n =
        oneof
          [ elements [Top, Bottom]
          , genAtom
          , liftM Not sub
          , liftM2 (BinOp And) sub sub
          , liftM2 (BinOp Or) sub sub
          , liftM2 (BinOp Implies) sub sub
          , liftM2 (BinOp Xor) sub sub
          , liftM2 (BinOp Iff) sub sub]
            where sub = fol' (n `div` 2)

instance Arbitrary Symbols where
  arbitrary =  elements [long, shouting, symbolic, semisymbolic]

-- Tests if printing a formula and parsing the result yields back the original formula.
prop_parsing_back :: Symbols -> FOL String -> Bool
prop_parsing_back s f = case parseFOL (prettyPrintFm s f) of
  Left _   -> False
  Right f' -> f == f'

-- Make sure Ord and Eq fit together.
prop_fol_ord :: FOL String -> FOL String -> Bool
prop_fol_ord f0 f1 = case f0 `compare` f1 of
  EQ -> f0 == f1
  _  -> f0 /= f1

-- Equal to self.
prop_fol_self_eq :: FOL String -> Bool
prop_fol_self_eq f = f `compare` f == EQ && f == f
