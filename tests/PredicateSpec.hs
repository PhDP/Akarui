{-# LANGUAGE FlexibleInstances #-}

module PredicateSpec where

import Test.QuickCheck
import Sphinx.Predicate

import TextGen
import TermSpec

genPredicate :: Gen (Predicate String)
genPredicate = do
  name <- genPascalString
  args <- genTerms
  return $ Predicate name args

instance Arbitrary (Predicate String) where
  arbitrary = genPredicate

prop_predicate_eq_itself :: Predicate String -> Bool
prop_predicate_eq_itself p0 = p0 == p0

prop_predicate_cmp_itself :: Predicate String -> Bool
prop_predicate_cmp_itself p0 = p0 `compare` p0 == EQ

-- Make sure Ord and Eq fit together.
prop_predicate_ord :: Predicate String -> Predicate String -> Bool
prop_predicate_ord p0 p1 = case p0 `compare` p1 of
  EQ -> p0 == p1
  _  -> p0 /= p1
