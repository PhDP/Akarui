module Faun.FOL.BinT where

import Faun.ShowTxt

-- | Supported binary connectives (in order of precedence).
data BinT =
  -- | Conjunction. Returns true only if both sides are true.
    And
  -- | Disjunction. Returns true if at least one operand is true.
  | Or
  -- | Implication is... messed up. It returns true except if the
  -- left operand is true and the right one is false, e.g. True implies False
  -- is the only situation where implication returns false.
  | Implies
  -- | Exclusive disjunction. Returns true if one and only one operand is true.
  | Xor
  -- | Equivalence. Returns true is both operand have the same value, i.e. both
  -- true or both are false.
  | Iff
  deriving (Eq, Ord, Show)

instance ShowTxt BinT where
  showTxt And = "And"
  showTxt Or = "Or"
  showTxt Implies = "Implies"
  showTxt Xor = "Xor"
  showTxt Iff = "Iff"
