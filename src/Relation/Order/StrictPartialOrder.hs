module Relation.Order.StrictPartialOrder where

import Prelude hiding ((<))

--------------------------------------------

class StrictPartialOrder a where
  (<) :: a -> a -> Bool -- ^ Precedence

  (>) :: a -> a -> Bool -- ^ After
  x > y = y < x
