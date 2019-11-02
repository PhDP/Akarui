module Akarui.FOL.PrettyPrint
( PrettyPrint(..)
) where

import qualified Data.Text as T
import Akarui.FOL.Symbols

class PrettyPrint t where
  prettyPrint :: Symbols -> t -> T.Text
