{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)

import PropLogicSpec
import PredicateSpec

main :: IO ()
main = do
  let
    tests =
      [ quickCheckResult prop_simplify_idempotent
      , quickCheckResult prop_eval_simplify
      , quickCheckResult prop_eval_atoms
      , quickCheckResult prop_predicate_eq_itself
      , quickCheckResult prop_predicate_cmp_itself
      , quickCheckResult prop_predicate_ord]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
