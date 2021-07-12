{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.CyclicOrder.Laws (laws) where

import Data.Bits (xor)  
import Data.Proxy (Proxy)
import Prelude hiding (cycle)
import Test.QuickCheck hiding ((===))

import Test.Relation.Identity
import Test.Relation.Order.CyclicOrder
import Test.Relation.Order.PartialCyclicOrder

---------------------------------------------------------

type Constraints a = (Arbitrary a, CyclicOrder a, Show a)

-- | Cyclic Order Properties :
prop_total :: forall a. Constraints a => Proxy a -> Property
prop_total _ = property $ 
    \(x :: a, y :: a, z :: a) -> ((x === y) || (y === z) || (x === z)) `xor` cycle x y z  `xor` cycle z y x

--------------------------

laws :: Constraints a => Proxy a -> [(String, Property)]
laws proxy = [ ("Total", prop_total proxy) ] 
