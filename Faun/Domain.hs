-- | Domain of terms.
module Faun.Domain where

import Data.Set (Set)

-- | Domain of variables and constants.
data Domain =
    Any
  | Interval Double Double
  | Finite (Set String)
