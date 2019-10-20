-- | Faun.Fuzzy is a fun functional set of functions for fuzzy sets.
module Faun.MVL.Truth
( Fuzzy()
, mkFuzzy
, invNeg
, toDouble
, isFalse
, isTrue
, nonZero
, fuzzyFalse
, fuzzyTrue
, Fuzzy2()
, mkFuzzyInterval
) where

data Fuzzy = Fuzzy Double
  deriving (Eq, Ord, Show)

-- | Builds a fuzzy value, ensuring it is in the [0, 1] range.
mkFuzzy :: Double -> Fuzzy
mkFuzzy = Fuzzy . unit

-- | Involutive negation.
invNeg :: Fuzzy -> Fuzzy
invNeg (Fuzzy x) = Fuzzy $ 1 - x

toDouble :: Fuzzy -> Double
toDouble (Fuzzy x) = x

isFalse :: Fuzzy -> Bool
isFalse (Fuzzy x) = x == 0

isTrue :: Fuzzy -> Bool
isTrue (Fuzzy x) = x == 1

nonZero :: Fuzzy -> Bool
nonZero (Fuzzy x) = x /= 0

fuzzyFalse :: Fuzzy
fuzzyFalse = Fuzzy 0

fuzzyTrue :: Fuzzy
fuzzyTrue = Fuzzy 1

data Fuzzy2 =
    FuzzyInterval Fuzzy Fuzzy
--  | FuzzyFun (Fuzzy -> Fuzzy)

mkFuzzyInterval :: Double -> Double -> Fuzzy2
mkFuzzyInterval x y = if x > y then mkFuzzyInterval y x else FuzzyInterval (mkFuzzy x) (mkFuzzy y)

{-# INLINE unit #-}
unit :: Double -> Double
unit x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
