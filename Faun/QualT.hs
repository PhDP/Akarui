module Faun.QualT where

import Faun.ShowTxt

-- | Supported qualifiers. They are only used in some logics, for example they
-- make no sense in propositional logic.
data QualT =
  -- | Univeral qualifier.
    ForAll
  -- | Existential qualifier.
  | Exists
  | Unique
  deriving (Eq, Ord, Show)

instance ShowTxt QualT where
  showTxt ForAll = "ForAll"
  showTxt Exists = "Exists"
  showTxt Unique = "Unique"
