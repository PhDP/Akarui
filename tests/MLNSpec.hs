{-# LANGUAGE OverloadedStrings #-}

module MLNSpec where

import Sphinx.MLN
import Sphinx.FOL
import Sphinx.Parser
import Sphinx.Symbols

-- Tests if printing a formula plus a weight and parsing the result yields back
-- the original formula and weight. It should fail if w is negative, because
-- it doesn't make sense.
prop_w_parsing_back :: Symbols -> FOL String -> Double -> Bool
prop_w_parsing_back s f w = case parseWFOL (showWFormula s f w) of
  Left _         -> w < 0.0
  Right (f', w') -> f == f' && feq w w'
  where
    -- Very generous float equality.
    feq a b = abs (a - b) <= 0.05 * max a b
