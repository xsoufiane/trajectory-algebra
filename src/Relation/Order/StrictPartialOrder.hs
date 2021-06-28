module Relation.Order.StrictPartialOrder where

import Prelude hiding ((<))
import qualified Prelude as P ((<))

--------------------------------------------

class StrictPartialOrder a where
    (<) :: a -> a -> Bool -- ^ Precedence
    
    (>) :: a -> a -> Bool -- ^ After
    x > y = y < x

-- | Instances
instance StrictPartialOrder Int where
    x < y = (P.<) x y
    