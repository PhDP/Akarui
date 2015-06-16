{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- Investigate, perhaps a bad idea.

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad
import System.Exit (exitFailure)

--import Data.Char (isLower)
--import Sphinx.FOL
import Sphinx.Formula
--import Sphinx.Predicate

genLowerChar :: Gen Char
genLowerChar = elements ['a'..'z']

genUpperChar :: Gen Char
genUpperChar = elements ['A'..'Z']

genPascalChar :: Gen Char
genPascalChar = oneof [genLowerChar, genUpperChar]

genPascalString :: Gen String
genPascalString = listOf genPascalChar

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
genProposition :: Gen (Formula String)
genProposition = do
  name <- genPascalString
  return (Atom name)

instance Arbitrary (Formula String) where
  arbitrary = sized fol'
    where
      fol' 0 = elements [Top, Bottom]
      fol' n | n > 0 =
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

prop_simplify_idempotent :: Formula String -> Bool
prop_simplify_idempotent f = let f' = simplify f in f' == simplify f'

main :: IO ()
main = do
  let tests = [ quickCheckResult prop_simplify_idempotent ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
