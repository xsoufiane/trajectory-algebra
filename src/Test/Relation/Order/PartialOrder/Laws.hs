{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.PartialOrder.Laws (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<=), (>=))
import Test.QuickCheck

import Test.Relation.Order.PartialOrder

-------------------------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, PartialOrder a, Show a)

-- | Partial Order Properties :
prop_reflexive :: forall a. Constraints a => Property
prop_reflexive = property (\(x :: a) -> x <= x)

prop_antisymmetric :: forall a. Constraints a => Property
prop_antisymmetric = property $ \(x :: a, y :: a) -> not (x <= y) || not (y <= x) || x == y

prop_transitive :: forall a. Constraints a => Property
prop_transitive = forAll gen $ \(x, _, z) -> x <= z
  where gen :: Gen (a, a, a)
        gen = suchThat (arbitrary :: Gen (a, a, a)) $ \(x, y, z) -> x <= y && y <= z

---------------------------------

laws :: (Arbitrary a, PartialOrder a, Show a) => Proxy a -> [(String, Property)]
laws proxy =
    [ ("Reflexive", prop_reflexive)
    , ("Antisymmetric", prop_antisymmetric)
    , ("Transitive", prop_transitive)
    ]
  where ?proxy = proxy
