{-# LANGUAGE FlexibleInstances #-}

module TermSpec where

import Test.QuickCheck
import Sphinx.Term
import TextGen

instance Arbitrary (Term String) where
  arbitrary = oneof [genVar, genConst, genFun]

genTerms :: Gen [Term String]
genTerms = sized $ \n -> do
  size <- choose (0, n `mod` 6) -- Huge lists of arguments make the test result hard to read / interpret.
  vectorOf size arbitrary

genVar :: Gen (Term String)
genVar = do
  name <- genCamelString
  return (Variable name)

genConst :: Gen (Term String)
genConst = do
  name <- genPascalString
  return (Constant name)

genFun :: Gen (Term String)
genFun = do
  name <- genPascalString
  args <- genTerms
  return $ Function name args
