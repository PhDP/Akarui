{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)

import PropLogic

-- For first-order logic (later):

--genCamelString :: Gen String
--genCamelString = suchThat genPascalString (isLower . head)
--
--genTerm :: Gen (Term String)
--genTerm = oneof [genVar, genConst, genFun]
--
--genVar :: Gen (Term String)
--genVar = do
--  name <- genCamelString
--  return (Variable name)
--
--genConst :: Gen (Term String)
--genConst = do
--  name <- genPascalString
--  return (Constant name)
--
--genFun :: Gen (Term String)
--genFun = do
--  name <- genPascalString
--  -- args <- list of terms
--  return (Function name [])
--
--genPredicate :: Gen (FOL String)
--genPredicate = do
--  name <- genPascalString
--  return (Atom (Predicate name []))
--

main :: IO ()
main = do
  let
    tests =
      [ quickCheckResult prop_simplify_idempotent
      , quickCheckResult prop_eval_simplify
      , quickCheckResult prop_eval_atoms ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
