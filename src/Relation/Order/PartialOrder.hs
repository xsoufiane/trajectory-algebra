module Relation.Order.PartialOrder where

import Prelude hiding ((<), (<=))

import Relation.Order.StrictPartialOrder

--------------------------------------------

class (Eq a, StrictPartialOrder a) => PartialOrder a where
  (<=) :: a -> a -> Bool
  x <= y = x == y || x < y
  
  (>=) :: a -> a -> Bool
  x >= y = y <= x

-- | Instances
instance PartialOrder Int