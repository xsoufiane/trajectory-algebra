module Chronon
    ( -- * Types
      Chronon(unChronon)

     -- * Constructors
    , chronon
    ) where

import Prelude hiding ((<))

import LinearOrder

----------------------------------------------------------------------    
  
newtype Chronon a = Chronon { unChronon :: a } deriving (Show)

instance LinearOrder a => LinearOrder (Chronon a) where
  Chronon x < Chronon y = x < y 
  Chronon x === Chronon y = x === y

-- | constructors 
chronon :: LinearOrder a => a -> Chronon a
chronon = Chronon
