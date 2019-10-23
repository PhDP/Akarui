module Faun.MVL.Fuzzy
( Fuzzy()
, mkFuzzy
, invNeg
, toDouble
, isFalse
, isTrue
, nonZero
, fuzzyFalse
, fuzzyTrue
) where

import Faun.MVL.Truth

data Fuzzy = Fuzzy Double
  deriving (Eq, Ord, Show)

instance Truth Fuzzy where
  order _ = 1

  isFalse (Fuzzy x) = x == 0

  isTrue (Fuzzy x) = x == 1

-- | Builds a fuzzy value, ensuring it is in the [0, 1] range.
mkFuzzy :: Double -> Fuzzy
mkFuzzy = Fuzzy . unit

-- | Involutive negation.
invNeg :: Fuzzy -> Fuzzy
invNeg (Fuzzy x) = Fuzzy $ 1 - x

toDouble :: Fuzzy -> Double
toDouble (Fuzzy x) = x

nonZero :: Fuzzy -> Bool
nonZero (Fuzzy x) = x /= 0

fuzzyFalse :: Fuzzy
fuzzyFalse = Fuzzy 0

fuzzyTrue :: Fuzzy
fuzzyTrue = Fuzzy 1

{-# INLINE unit #-}
unit :: Double -> Double
unit x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
