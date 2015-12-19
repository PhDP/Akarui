module Faun.PrettyPrint
( PrettyPrint(..)
) where

import qualified Data.Text as T
import Faun.Symbols

class PrettyPrint t where
  prettyPrint :: Symbols -> t -> T.Text
