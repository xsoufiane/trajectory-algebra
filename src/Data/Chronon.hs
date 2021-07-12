{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Chronon
    ( -- * Types  
      Chronon
             
      -- * Observations
    , ChrononOps((<), (>), (===), (<=), (>=), _cycle)
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

    _cycle :: t -> t -> t -> Bool
