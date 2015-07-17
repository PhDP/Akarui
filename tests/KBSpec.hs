{-# LANGUAGE FlexibleInstances #-}

module KBSpec where

import Test.QuickCheck
import qualified Data.Set as Set
--import Data.Set (Set)
import Sphinx.KB
--import Sphinx.Formula
import PropLogicSpec

-- The problem is that the formula don't share any atoms. It would be wiser
-- to generate a list of atoms and all formula would pick from the same pool
-- so they can share atoms.
genFMs :: Gen (KB String)
genFMs = sized $ \n -> do
  size <- choose (1, n `mod` 32)
  ls <- vectorOf size genPropFormula
  return $ Set.fromList ls

-- For testing a knowledge base made of propositional logic formulas.
instance Arbitrary (KB String) where
  arbitrary = genFMs
