module Faun.QuanT where

import Faun.ShowTxt

-- | Supported quantifiers. They are only used in some logics, for example they
-- make no sense in propositional logic.
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
