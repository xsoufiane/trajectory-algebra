{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Chronon
    ( -- * Types  
      Chronon
             
      -- * Observations
    , ChrononOps((<), (>), (===), (<=), (>=))
    , CyclicChronon(cycle)
    ) where

import Prelude hiding ((<), (<=))  
  
----------------------------------------------------------------------    

data family Chronon :: k

-- | Observations
class ChrononOps t where
    (<) :: t -> t -> Bool
     
    (>) :: t -> t -> Bool
    x > y = y < x

    (===) :: t -> t -> Bool

    (<=) :: Eq t => t -> t -> Bool
    x <= y = x < y || x == y

    (>=) :: Eq t => t -> t -> Bool
    x >= y = y <= x
    
class CyclicChronon t where    
    cycle :: t -> t -> t -> Bool
