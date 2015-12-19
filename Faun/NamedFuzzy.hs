module Faun.NamedFuzzy
( NamedFuzzy(..)
, showNamedFuzzy
) where

import qualified Data.Text as T
import Faun.FuzzySet
import Faun.ShowTxt
import Faun.PrettyPrint

-- | Named fuzzy set.
data NamedFuzzy = NamedFuzzy
  {  name :: T.Text
  ,  set :: FuzzySet
  }

instance Show NamedFuzzy where
  show = T.unpack . name

instance ShowTxt NamedFuzzy where
  showTxt = name

instance PrettyPrint NamedFuzzy where
  prettyPrint _ = name

-- | Show both the name and the fuzzy set.
showNamedFuzzy :: NamedFuzzy -> T.Text
showNamedFuzzy f = T.concat [name f, " = ", showTxt (set f)]
