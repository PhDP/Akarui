module Faun.FOL.PrettyPrint
( PrettyPrint(..)
) where

import qualified Data.Text as T
import Faun.FOL.Symbols

class PrettyPrint t where
  prettyPrint :: Symbols -> t -> T.Text
