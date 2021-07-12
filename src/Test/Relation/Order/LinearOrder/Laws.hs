{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.LinearOrder.Laws where

import Data.Bits (xor)
import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import Test.Relation.Identity ((===))
import Test.Relation.Order.LinearOrder (LinearOrder)
import Test.Relation.Order.StrictPartialOrder ((<), (>))

-----------------------------------------------------------------------------------------------

type Constraints a = (Arbitrary a, LinearOrder a, Show a)

-- | Linear Order Properties :
prop_linear :: forall a. Constraints a => Proxy a -> Property
prop_linear _ = property $ \(x :: a, y :: a) -> x < y `xor` x > y `xor` x === y

----------------------------------------------------

laws :: Constraints a => Proxy a -> [(String, Property)]
laws proxy = [ ("LinearOrder", prop_linear proxy) ]
