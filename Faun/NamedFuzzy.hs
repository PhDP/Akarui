module Faun.NamedFuzzy
( NamedFuzzy(..)
) where

import qualified Data.Text as T
import Faun.FuzzySet
import Faun.ShowTxt

-- | Named fuzzy set.
data NamedFuzzy = NamedFuzzy
  {  name :: T.Text
  ,  set :: FuzzySet
  }

instance Show NamedFuzzy where
  show = T.unpack . name

instance ShowTxt NamedFuzzy where
  showTxt = name
