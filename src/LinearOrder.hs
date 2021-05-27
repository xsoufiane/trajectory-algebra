module LinearOrder where

import StrictPartialOrder

--------------------------------------------

class StrictPartialOrder a => LinearOrder a where
  (===) :: a -> a -> Bool -- ^ Identity
