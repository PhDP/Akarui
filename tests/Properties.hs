import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)
import PredicateSpec
import FOLSpec

main :: IO ()
main = do
  let
    tests =
      [ quickCheckResult prop_predicate_eq_itself
      , quickCheckResult prop_predicate_cmp_itself
      , quickCheckResult prop_predicate_ord
--      , quickCheckResult prop_parsing_back -- Will be reinstated once I figure out how to handle top/bot printing.
      , quickCheckResult prop_fol_ord
      , quickCheckResult prop_fol_self_eq
      ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
