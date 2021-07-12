{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.PartialCyclicOrder.Laws (laws) where

import Data.Proxy (Proxy)
import Prelude hiding (cycle)
import Test.QuickCheck 

import Test.Relation.Order.PartialCyclicOrder 
  
----------------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, PartialCyclicOrder a, Show a)

prop_cyclic :: forall a. Constraints a => Property
prop_cyclic = forAll condition $ \(x, y, z) -> cycle y z x
  where condition :: Gen (a, a, a)
        condition = suchThat arbitrary $ \(x, y, z) -> cycle x y z

prop_asymmetric :: forall a. Constraints a => Property
prop_asymmetric = forAll condition $ \(x, y, z) -> not $ cycle z y x
  where condition :: Gen (a, a, a)
        condition = suchThat arbitrary $ \(x, y, z) -> cycle x y z

prop_transitive :: forall a. Constraints a => Property
prop_transitive = forAll gen $ \(x, y, _, h) -> cycle x y h
  where gen :: Gen (a, a, a, a)
        gen = suchThat (arbitrary :: Gen (a, a, a, a)) $ \(x, y, z, h) -> cycle x y z && cycle x z h

--------------------------

laws :: (Arbitrary a, PartialCyclicOrder a, Show a) => Proxy a -> [(String, Property)]
laws proxy =
    [ ("Cyclic", prop_cyclic)
    , ("Asymmetric", prop_asymmetric)
    , ("Transitive", prop_transitive)
    ]
  where ?proxy = proxy  
