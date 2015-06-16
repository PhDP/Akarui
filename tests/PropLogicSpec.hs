{-# LANGUAGE FlexibleInstances #-} -- Investigate, perhaps a bad idea.

module PropLogicSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Control.Monad
import Sphinx.Formula

import TextGen

genProposition :: Gen (Formula String)
genProposition = do
  name <- genPascalString
  return (Atom name)

instance Arbitrary (Formula String) where
  arbitrary = sized fol'
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
prop_eval_simplify :: Formula String -> Bool
prop_eval_simplify f = eval ass f == eval ass (simplify f)
  where ass = randomFairAssF 42 f
