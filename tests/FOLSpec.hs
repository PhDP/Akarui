{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module FOLSpec where

import Test.QuickCheck
import Control.Monad
import Sphinx.Formula
import Sphinx.FOL
import Sphinx.Parser
import Sphinx.Symbols
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
