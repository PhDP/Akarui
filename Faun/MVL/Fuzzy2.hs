-- | Module for type-2 fuzzy logic, where the fuzziness itself is fuzzy.
module Faun.MVL.Fuzzy2
( Fuzzy2()
, mkFuzzyInterval
, uncertainty
) where

import Faun.MVL.Truth
import Faun.MVL.Fuzzy

data Fuzzy2 =
    FuzzyInterval Fuzzy Fuzzy
--  | FuzzyFun (Fuzzy -> Fuzzy)

instance Truth Fuzzy2 where
  order _ = 2

  isFalse (FuzzyInterval x y) = isFalse x && isFalse y

  isTrue (FuzzyInterval x y) = isTrue x && isTrue y

mkFuzzyInterval :: Double -> Double -> Fuzzy2
mkFuzzyInterval x y = if x > y then mkFuzzyInterval y x else FuzzyInterval (mkFuzzy x) (mkFuzzy y)

uncertainty :: Fuzzy -> Fuzzy2 -> Fuzzy
uncertainty x (FuzzyInterval a b) = if a <= x && x <= b then fuzzyTrue else fuzzyFalse
