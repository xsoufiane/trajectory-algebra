module LinearOrder
    ( -- * Types
      LinearOrder(..)
    ) where

import Prelude hiding ((<))
import qualified Prelude as P ((<))

--------------------------------------------

-- + TypeClass  
class LinearOrder a where
  (<) :: a -> a -> Bool -- ^ Precedence

  (>) :: a -> a -> Bool -- ^ After
  x > y = y < x

  (===) :: a -> a -> Bool -- ^ Identity

  betweenness :: a -> a -> a -> Bool
  betweenness x y z = y < x && x < z || z < x && x < y
  
-- + Instances  
instance LinearOrder Int where
  (<) = (P.<)
  (===) = (==)
