module Akarui.FOL.QuanT where

import Akarui.ShowTxt

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
