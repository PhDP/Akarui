-- | Defines a type class for clausal forms.
module Faun.LiteralSign where

import Faun.Symbols

data LiteralSign = Positive | Negative
  deriving (Eq, Ord, Show, Read)

-- | Pretty print a literal sign. It prints nothing for positive literals, and
-- the not sign for negative literals.
prettyPrint :: Symbols -> LiteralSign -> String
prettyPrint _ Positive = ""
prettyPrint s Negative = symNot s
