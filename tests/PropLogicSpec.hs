{-# LANGUAGE FlexibleInstances #-}

module PropLogicSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Control.Monad
import System.Random
import Sphinx.Formula

import TextGen

genProposition :: Gen (Formula String)
genProposition = do
  name <- genPascalString
  return (Atom name)

genPropFormula :: Gen (Formula String)
genPropFormula = sized fol'
  where
    fol' 0 = elements [Top, Bottom]
    fol' n =
      oneof
        [ elements [Top, Bottom]
        , genProposition
        , liftM Not sub
        , liftM2 (BinOp And) sub sub
        , liftM2 (BinOp Or) sub sub
        , liftM2 (BinOp Implies) sub sub
        , liftM2 (BinOp Xor) sub sub
        , liftM2 (BinOp Iff) sub sub]
          where sub = fol' (n `div` 2)

instance Arbitrary (Formula String) where
  arbitrary = genPropFormula

-- Simplification is idempotent.
prop_simplify_idempotent :: Formula String -> Bool
prop_simplify_idempotent f = let f' = simplify f in f' == simplify f'

-- Evaluation simplifies to Top/Bottom unless atoms are present.
prop_eval_atoms :: Formula String -> Bool
prop_eval_atoms f = case eval Map.empty f of
  Top    -> True
  Bottom -> True
  _      -> Set.size (atoms f) > 0

-- Simplification does not change evaluation.
prop_eval_simplify :: Int -> Formula String -> Bool
prop_eval_simplify seed f = eval ass f == eval ass (simplify f)
  where ass = randomFairAssF (mkStdGen seed) f

-- Make sure Ord and Eq fit together.
prop_proplog_ord :: Formula String -> Formula String -> Bool
prop_proplog_ord f0 f1 = case f0 `compare` f1 of
  EQ -> f0 == f1
  _  -> f0 /= f1

-- CoreOp is idempotent.
prop_coreOp_idempotent :: Formula String -> Bool
prop_coreOp_idempotent f = let f' = coreOp f in f' == coreOp f'

-- CoreOp does not change evaluation.
prop_eval_coreOp :: Int -> Formula String -> Bool
prop_eval_coreOp seed f = eval ass f == eval ass (coreOp f)
  where ass = randomFairAssF (mkStdGen seed) f
