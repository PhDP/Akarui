module Sphinx.TypedObj where

data TypedObj = TypedObj
  { objName :: String
  , objType :: String}

instance Show TypedObj where
  show t = objName t ++ ": " ++ objType t
