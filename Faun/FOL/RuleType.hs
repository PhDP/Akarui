-- | A knowledge base is a set of formulas. Will replace KB.
module Faun.FOL.RuleType where

-- | Supported types of logic formulas.
data RuleType =
  -- | Probabilistic rule, those with a weight (not always a probability).
    Probabilistic Double
  -- | Hard rules, those expected to be true all the time.
  | Hard
  -- | Unknown rule type.
  | Unknown

instance Show RuleType where
  show (Probabilistic p) = " ," ++ show p ++ "."
  show Hard = "."
  show Unknown = "?"
