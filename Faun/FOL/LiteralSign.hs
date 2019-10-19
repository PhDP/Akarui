-- | Defines a type class for clausal forms.
module Faun.FOL.LiteralSign where

import Faun.ShowTxt
import Faun.FOL.Symbols
import Faun.FOL.PrettyPrint

data LiteralSign = Positive | Negative
  deriving (Eq, Ord, Show, Read)

instance ShowTxt LiteralSign where
  showTxt Positive = "Positive"
  showTxt Negative = "Negative"

instance PrettyPrint LiteralSign where
  prettyPrint _ Positive = ""
  prettyPrint s Negative = symNot s
