{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearOrderSpec (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Classes

import LinearOrder
import StrictPartialOrder

---------------------------------------------------------------------------------------------

type Constraints a = (Arbitrary a, LinearOrder a, Show a) 
type PropertySig a = Proxy a -> Property

-- | Precedence Laws :
prop_precedenceLinearity :: forall a. Constraints a => PropertySig a
prop_precedenceLinearity _ = property $ \(x :: a, y :: a) -> x < y || x > y || x === y

prop_precedenceXY :: forall a. Constraints a => PropertySig a
prop_precedenceXY _ = property $ \(x :: a, y :: a) -> (x < y) == (not (x > y) && not (x === y))

-- | Identity Laws :
prop_identityReflexive :: forall a. Constraints a => PropertySig a
prop_identityReflexive _ = property $ \(x :: a) -> x === x

prop_identitySymmetric :: forall a. Constraints a => PropertySig a
prop_identitySymmetric _ =
  forAll (suchThat (arbitrary :: Gen (a, a)) $ uncurry (===)) $ \(x, y) -> y === x

prop_identityTransitive :: forall a. Constraints a => PropertySig a
prop_identityTransitive _ =
  forAll gen (\(x, _, z) -> x === z) 
  where
    gen :: Gen (a, a, a) 
    gen = (\x -> (x, x, x)) <$> (arbitrary :: Gen a)

prop_identityX :: forall a. Constraints a => PropertySig a
prop_identityX _ = property $ \(x :: a, y :: a) -> x === y == (not (x < y) && not (x > y))

laws :: (Arbitrary a, LinearOrder a, Show a) => Proxy a -> Laws
laws p = Laws "LinearOrder" 
  [ ("Precedence Linear", prop_precedenceLinearity p)
  , ("if x < y, then x y are non-identical and x > y not true", prop_precedenceXY p)
  , ("Identity Reflexive", prop_identityReflexive p)
  , ("Identity Symmetric", prop_identitySymmetric p)
  , ("Identity Transitive", prop_identityTransitive p)
  , ("If X Y are identical, then they can't be comapred", prop_identityX p)
  ]
  
---- | Betweenness  Laws :
--prop_betweenness :: forall a. Constraints a => PropertySig a
--prop_betweenness _ = property $ \(x :: a, y :: a, z :: a) -> betweenness x y z == (y < x && x < z || z < x && x < y)
--
--prop_betweennessXYZ :: forall a. Constraints a => PropertySig a
--prop_betweennessXYZ _ = property $ \(x :: a, y :: a, z :: a) -> betweenness x y z == betweenness x z y
--
--prop_betweennessXXY :: forall a. Constraints a => PropertySig a
--prop_betweennessXXY _ = property $ \(x :: a, y :: a) -> not $ betweenness x x y 
--
--prop_betweennessXYY :: forall a. Constraints a => PropertySig a
--prop_betweennessXYY _ = property $ \(x :: a, y :: a) -> not $ betweenness x y y
-- , ("Betweenness Definition", prop_betweenness p)
--  , ("Betweenness 2nd 3rd args are commutative", prop_betweennessXYZ p)
--  , ("X is between X Y", prop_betweennessXXY p)
--  , ("X is not between Y Y", prop_betweennessXYY p)