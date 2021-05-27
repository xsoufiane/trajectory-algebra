{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Chronon where
  
import Test.QuickCheck

import Prelude hiding ((<))
import qualified Prelude as P ((<))

import Begin
import Chronon
import End
import StrictPartialOrder
import LinearOrder

---------------------------------------------

data instance Chronon Int = Chronon Int deriving (Show)

-- | Instances:
instance Arbitrary (Chronon Int) where
  arbitrary = Chronon <$> arbitrary

instance StrictPartialOrder (Chronon Int) where
  Chronon x < Chronon y = (P.<) x y

instance LinearOrder (Chronon Int) where
  Chronon x === Chronon y = x == y

instance Begin
instance End
