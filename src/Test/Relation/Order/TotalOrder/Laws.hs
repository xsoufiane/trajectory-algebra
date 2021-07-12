{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.TotalOrder.Laws where

import Data.Proxy (Proxy)
import Prelude hiding ((<=), (>=))
import Test.QuickCheck

import Test.Relation.Order.PartialOrder ((<=), (>=))
import Test.Relation.Order.TotalOrder

-----------------------------------------------------------------------------------------------

type Constraints a = (Arbitrary a, TotalOrder a, Show a)

-- | Total Order Properties :
prop_total :: forall a. Constraints a => Proxy a -> Property
prop_total _ = property $ \(x :: a, y :: a) -> x <= y || x >= y

---------------------------------------

laws :: Constraints a => Proxy a -> [(String, Property)]
laws proxy = [ ("TotalOrder", prop_total proxy) ]
  