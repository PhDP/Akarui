-- | Defines a type class for clausal forms.
module Akarui.FOL.LiteralSign where

import Akarui.ShowTxt
import Akarui.FOL.Symbols
import Akarui.FOL.PrettyPrint

data LiteralSign = Positive | Negative
  deriving (Eq, Ord, Show, Read)

instance ShowTxt LiteralSign where
  showTxt Positive = "Positive"
  showTxt Negative = "Negative"

instance PrettyPrint LiteralSign where
  prettyPrint _ Positive = ""
  prettyPrint s Negative = symNot s
