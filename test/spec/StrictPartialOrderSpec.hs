{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StrictPartialOrderSpec (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Classes  
    
import StrictPartialOrder  

--------------------------------------------

type Constraints a = (Arbitrary a, StrictPartialOrder a, Show a)
type PropertySig a = Proxy a -> Property

-- | Precedence Laws :
prop_precedenceIrreflexive :: forall a. Constraints a => PropertySig a
prop_precedenceIrreflexive _ = property (\(x :: a) ->  not $ x < x)

prop_precedenceAsymmetric :: forall a. Constraints a => PropertySig a
prop_precedenceAsymmetric _ =
  forAll condition $ \(x, y) -> not $ y < x
  where
    condition :: Gen (a, a)
    condition = suchThat arbitrary $ uncurry (<)

prop_precedenceTransitive :: forall a. Constraints a => PropertySig a
prop_precedenceTransitive _ =
  forAll gen $ \(x, _, z) -> x < z
  where
    gen :: Gen (a, a, a)
    gen = suchThat (arbitrary :: Gen (a, a, a)) $ \(x, y, z) -> x < y && y < z

laws :: (Arbitrary a, StrictPartialOrder a, Show a) => Proxy a -> Laws
laws p = Laws "StrictPartialOrder"
  [ ("Precedence Irreflexive", prop_precedenceIrreflexive p)
  , ("Precedence Asymmetric", prop_precedenceAsymmetric p)
  , ("Precedence Transitive", prop_precedenceTransitive p)
  ]
