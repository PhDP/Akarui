-- | Defines a type class for clausal forms.
module Faun.LiteralSign where

import Faun.Symbols
import Faun.ShowTxt
import Faun.PrettyPrint

data LiteralSign = Positive | Negative
  deriving (Eq, Ord, Show, Read)

instance ShowTxt LiteralSign where
  showTxt Positive = "Positive"
  showTxt Negative = "Negative"

instance PrettyPrint LiteralSign where
  prettyPrint _ Positive = ""
  prettyPrint s Negative = symNot s
