module PartialOrder where

import Prelude hiding ((<=))

--------------------------------------------

-- + TypeClass
class (Eq a) => PartialOrder a where
  (<=) :: a -> a -> Bool

  (>=) :: a -> a -> Bool
  x >= y = y <= x
