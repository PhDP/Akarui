-- | A module to support (in the future) typed first-order logic.
module Sphinx.TypedObj where

-- | An object with a type.
data TypedObj = TypedObj
  { objName :: String
  , objType :: String}

instance Show TypedObj where
  show t = objName t ++ ": " ++ objType t
