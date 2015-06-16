{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)

import PropLogicSpec

main :: IO ()
main = do
  let
    tests =
      [ quickCheckResult prop_simplify_idempotent
      , quickCheckResult prop_eval_simplify
      , quickCheckResult prop_eval_atoms ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
