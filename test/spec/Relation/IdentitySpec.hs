{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relation.IdentitySpec where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((===))

import Relation.Identity

-------------------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, Identity a, Show a) 
  
-- | Identity Properties :
prop_reflexive :: forall a. Constraints a => Property
prop_reflexive = property $ \(x :: a) -> x === x

prop_symmetric :: forall a. Constraints a => Property
prop_symmetric = forAll (suchThat (arbitrary :: Gen (a, a)) $ uncurry (===)) $ \(x, y) -> y === x

prop_transitive :: forall a. Constraints a => Property
prop_transitive = forAll gen (\(x, _, z) -> x === z) 
  where gen :: Gen (a, a, a) 
        gen = (\x -> (x, x, x)) <$> (arbitrary :: Gen a)


laws :: (Arbitrary a, Identity a, Show a) => Proxy a -> [(String, Property)]
laws proxy =
    [ ("Irreflexive", prop_reflexive)
    , ("Asymmetric", prop_symmetric)
    , ("Transitive", prop_transitive)
    ]
  where ?proxy = proxy
