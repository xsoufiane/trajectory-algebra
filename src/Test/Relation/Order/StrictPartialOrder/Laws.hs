{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Relation.Order.StrictPartialOrder.Laws (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck

import Test.Relation.Order.StrictPartialOrder  

---------------------------------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, StrictPartialOrder a, Show a)

-- | Strict Partial Order Properties :
prop_irreflexive :: forall a. Constraints a => Property
prop_irreflexive = property (\(x :: a) -> not $ x < x)

prop_asymmetric :: forall a. Constraints a => Property
prop_asymmetric = forAll condition $ \(x, y) -> not $ y < x
  where condition :: Gen (a, a)
        condition = suchThat arbitrary $ uncurry (<)

prop_transitive :: forall a. Constraints a => Property
prop_transitive = forAll gen $ \(x, _, z) -> x < z
  where gen :: Gen (a, a, a)
        gen = suchThat (arbitrary :: Gen (a, a, a)) $ \(x, y, z) -> x < y && y < z

--------------------------------

laws :: (Arbitrary a, StrictPartialOrder a, Show a) => Proxy a -> [(String, Property)]
laws proxy =
    [ ("Irreflexive", prop_irreflexive)
    , ("Asymmetric", prop_asymmetric)
    , ("Transitive", prop_transitive)
    ]
  where ?proxy = proxy
