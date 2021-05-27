{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearOrderSpec (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import LinearOrder
import StrictPartialOrder

---------------------------------------------------------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, LinearOrder a, Show a) 

-- | Precedence Laws :
prop_precedenceLinearity :: forall a. Constraints a => Property
prop_precedenceLinearity = property $ \(x :: a, y :: a) -> x < y || x > y || x === y

prop_precedenceXY :: forall a. Constraints a => Property
prop_precedenceXY = property $ \(x :: a, y :: a) -> (x < y) == (not (x > y) && not (x === y))

-- | Identity Laws :
prop_identityReflexive :: forall a. Constraints a => Property
prop_identityReflexive = property $ \(x :: a) -> x === x

prop_identitySymmetric :: forall a. Constraints a => Property
prop_identitySymmetric = forAll (suchThat (arbitrary :: Gen (a, a)) $ uncurry (===)) $ \(x, y) -> y === x

prop_identityTransitive :: forall a. Constraints a => Property
prop_identityTransitive = forAll gen (\(x, _, z) -> x === z) 
  where gen :: Gen (a, a, a) 
        gen = (\x -> (x, x, x)) <$> (arbitrary :: Gen a)

prop_identityX :: forall a. Constraints a => Property
prop_identityX = property $ \(x :: a, y :: a) -> x === y == (not (x < y) && not (x > y))

laws :: Constraints a => [(String, Property)]
laws =
  [ ("Precedence Linear", prop_precedenceLinearity)
  , ("if x < y, then x y are non-identical and x > y not true", prop_precedenceXY)
  , ("Identity Reflexive", prop_identityReflexive)
  , ("Identity Symmetric", prop_identitySymmetric)
  , ("Identity Transitive", prop_identityTransitive)
  , ("If X Y are identical, then they can't be comapred", prop_identityX)
  ]
  