module StrictPartialOrder where

import Prelude hiding ((<))

--------------------------------------------

-- + TypeClass
class StrictPartialOrder a where
  (<) :: a -> a -> Bool -- ^ Precedence

  (>) :: a -> a -> Bool -- ^ After
  x > y = y < x
