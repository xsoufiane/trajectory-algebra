{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relation.Order.LinearOrderSpec (laws) where

import Data.Bits (xor)
import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import Relation.Identity
import Relation.Order.LinearOrder
import Relation.Order.StrictPartialOrder

-----------------------------------------------------------------------------------------------

type Constraints a = (Arbitrary a, LinearOrder a, Show a)

-- | Linear Order Properties :
prop_linear :: forall a. Constraints a => Proxy a -> Property
prop_linear _ = property $ \(x :: a, y :: a) -> x < y `xor` x > y `xor` x === y


laws :: Constraints a => Proxy a -> [(String, Property)]
laws proxy = [ ("Linear", prop_linear proxy) ]
  