module Faun.ShowTxt
( ShowTxt(..)
) where

import qualified Data.Text as T

class (Show t) => ShowTxt t where
  showTxt :: t -> T.Text
  showTxt = T.pack . show
