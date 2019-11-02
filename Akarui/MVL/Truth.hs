module Akarui.MVL.Truth
( Truth(..)
) where

class Truth a where
  order :: a -> Int

  isFalse :: a -> Bool

  isTrue :: a -> Bool

  isNuanced :: a -> Bool
  isNuanced t = not (isFalse t) && not (isTrue t)

instance Truth Bool where
  order _ = 0

  isFalse x = not x

  isTrue x = x

  isNuanced _ = False
