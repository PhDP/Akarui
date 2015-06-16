{-# LANGUAGE OverloadedStrings #-}

module FOLSpec where

import Test.QuickCheck
import Sphinx.Formula
import Sphinx.FOL
import Sphinx.Predicate
import Sphinx.Term
import TextGen

genTerm :: Gen (Term String)
genTerm = oneof [genVar, genConst, genFun]

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
  -- args <- list of terms
  return (Function name [])

genPredicate :: Gen (FOL String)
genPredicate = do
  name <- genPascalString
  return (Atom (Predicate name []))
