{-# LANGUAGE FlexibleInstances #-}

module FOLSpec where

import qualified Data.Text as T
import Test.QuickCheck
import Control.Monad
import Faun.Formula
import Faun.FOL
import Faun.Symbols
import Faun.BinT
import Faun.Parser.FOL
import PredicateSpec

genAtom :: Gen FOL
genAtom = do
  p <- genPredicate
  return $ Atom p

-- Missing: existential and universal Quantifiers:
instance Arbitrary FOL where
  arbitrary = sized fol'
    where
      fol' 0 = elements [top, bot]
      fol' n =
        oneof
          [ elements [top, bot]
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
prop_parsing_back :: Symbols -> FOL -> Bool
prop_parsing_back s f = case parseFOL (T.unpack $ prettyPrintFm s f) of
  Left _   -> False
  Right f' -> f == f'

-- Make sure Ord and Eq fit together.
prop_fol_ord :: FOL -> FOL -> Bool
prop_fol_ord f0 f1 = case f0 `compare` f1 of
  EQ -> f0 == f1
  _  -> f0 /= f1

-- Equal to self.
prop_fol_self_eq :: FOL -> Bool
prop_fol_self_eq f = f `compare` f == EQ && f == f
