{-# LANGUAGE OverloadedStrings #-}

module MLNSpec where

import Manticore.MarkovLogic
import Manticore.FOL
import Manticore.Parser
import Manticore.Symbols

-- Tests if printing a formula plus a weight and parsing the result yields back
-- the original formula and weight. It should fail for negative numbers since
-- they don't make sense in this context.
prop_w_parsing_back :: Symbols -> FOL String -> Double -> Bool
prop_w_parsing_back s f w = case parseWFOL (fmtWFormula s f w) of
  Left _         -> w < 0.0
  Right (f', w') -> f == f' && feq w w'
  where
    -- Very generous float equality test, just to make sure the number is
    -- not completely wrong.
    feq a b = abs (a - b) <= 0.05 * max a b
