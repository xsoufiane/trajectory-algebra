module Relation.Order.PartialOrder where

import Prelude hiding ((<=), (<))

--------------------------------------------

class (Eq a) => PartialOrder a where
  (<=) :: a -> a -> Bool
  
  (>=) :: a -> a -> Bool
  x >= y = y <= x
  
  (<) :: a -> a -> Bool
  x < y = x /= y && x <= y
  
  (>) :: a -> a -> Bool
  x > y = y < x

