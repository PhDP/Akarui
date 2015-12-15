{-# LANGUAGE FlexibleInstances #-}

module PredicateSpec where

import Test.QuickCheck
import Faun.Predicate
import TextGen
import TermSpec
import qualified Data.Text as T

genPredicate :: Gen Predicate
genPredicate = do
  name <- genPascalString
  args <- genTerms
  return $ Predicate (T.pack name) args

instance Arbitrary Predicate where
  arbitrary = genPredicate

prop_predicate_eq_itself :: Predicate -> Bool
prop_predicate_eq_itself p0 = p0 == p0

prop_predicate_cmp_itself :: Predicate -> Bool
prop_predicate_cmp_itself p0 = p0 `compare` p0 == EQ

-- Make sure Ord and Eq fit together.
prop_predicate_ord :: Predicate -> Predicate -> Bool
prop_predicate_ord p0 p1 = case p0 `compare` p1 of
  EQ -> p0 == p1
  _  -> p0 /= p1
