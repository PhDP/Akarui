{-# LANGUAGE FlexibleInstances #-}

module TermSpec where

import Data.Text as T
import Test.QuickCheck
import Faun.Term
import TextGen

instance Arbitrary Term where
  arbitrary = oneof [genVar, genConst, genFun]

genTerms :: Gen [Term]
genTerms = sized $ \n -> do
  size <- choose (0, n `mod` 6) -- Huge lists of arguments make the test result hard to read / interpret.
  vectorOf size arbitrary

genVar :: Gen Term
genVar = do
  name <- genCamelString
  return $ Variable $ T.pack name

genConst :: Gen Term
genConst = do
  name <- genPascalString
  return $ Constant $ T.pack name

genFun :: Gen Term
genFun = do
  name <- genPascalString
  args <- genTerms
  return $ Function (T.pack name) args
