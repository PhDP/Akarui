module Faun.ShowTxt where

import qualified Data.Text as T

-- class (KnowledgeBase c) => Clause c where
class (Show t) => ShowTxt t where
  showTxt :: t -> T.Text
  showTxt = T.pack . show
