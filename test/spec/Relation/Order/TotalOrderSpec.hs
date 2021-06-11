{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relation.Order.TotalOrderSpec (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<=), (>=))
import Test.QuickCheck

import Relation.Order.TotalOrder
import Relation.Order.PartialOrder

-----------------------------------------------------------------------------------------------

type Constraints a = (Arbitrary a, TotalOrder a, Show a)

-- | Total Order Properties :
prop_total :: forall a. Constraints a => Proxy a -> Property
prop_total _ = property $ \(x :: a, y :: a) -> x <= y || x >= y


laws :: Constraints a => Proxy a -> [(String, Property)]
laws proxy = [ ("Total", prop_total proxy) ]
  