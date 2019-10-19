module Faun.FOL.QuanT where

import Faun.ShowTxt

-- | Supported quantifiers for predicate logics.
data QuanT =
  -- | Univeral quantifier.
    ForAll
  -- | Existential quantifier.
  | Exists
  -- | Unique quantifier.
  | Unique
  deriving (Eq, Ord, Show)

instance ShowTxt QuanT where
  showTxt ForAll = "ForAll"
  showTxt Exists = "Exists"
  showTxt Unique = "Unique"
