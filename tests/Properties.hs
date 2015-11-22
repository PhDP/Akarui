import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)

import PropLogicSpec
import PredicateSpec
import FOLSpec
import MLNSpec

main :: IO ()
main = do
  let
    tests =
      [ quickCheckResult prop_simplify_idempotent
      , quickCheckResult prop_eval_simplify
      , quickCheckResult prop_eval_atoms
      , quickCheckResult prop_predicate_eq_itself
      , quickCheckResult prop_predicate_cmp_itself
      , quickCheckResult prop_predicate_ord
      , quickCheckResult prop_parsing_back
      , quickCheckResult prop_proplog_ord
--      , quickCheckResult prop_w_parsing_back
      , quickCheckResult prop_fol_ord
      , quickCheckResult prop_fol_self_eq
      , quickCheckResult prop_coreOp_idempotent
      , quickCheckResult prop_eval_coreOp
      ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
