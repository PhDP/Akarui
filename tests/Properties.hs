{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Control.Monad
import Sphinx.Formula
import Sphinx.FOL

--pre :: Gen (Predicate String)

fol :: Gen (Formula (Predicate String))
fol = sized fol'
  where
    fol' 0 = elements [Top, Bottom]
    fol' n | n > 0 =
      oneof
        [ elements [Top, Bottom]
--        Predicate
        , liftM Not subfol
        , liftM2 (BinOp And) subfol subfol
        , liftM2 (BinOp Or) subfol subfol
        , liftM2 (BinOp Implies) subfol subfol
        , liftM2 (BinOp Xor) subfol subfol
        , liftM2 (BinOp Iff) subfol subfol]
          where subfol = fol' (n `div` 2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

prop_idempotent :: [Int] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

main :: IO ()
main = quickCheck prop_idempotent
