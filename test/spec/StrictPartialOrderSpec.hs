{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StrictPartialOrderSpec (laws) where

import Data.Proxy (Proxy)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===)) 
    
import StrictPartialOrder  

--------------------------------------------

type Constraints a = (?proxy :: Proxy a, Arbitrary a, StrictPartialOrder a, Show a)

-- | Precedence Laws :
prop_precedenceIrreflexive :: forall a. Constraints a => Property
prop_precedenceIrreflexive = property (\(x :: a) ->  not $ x < x)

prop_precedenceAsymmetric :: forall a. Constraints a => Property
prop_precedenceAsymmetric = forAll condition $ \(x, y) -> not $ y < x
  where condition :: Gen (a, a)
        condition = suchThat arbitrary $ uncurry (<)

prop_precedenceTransitive :: forall a. Constraints a => Property
prop_precedenceTransitive = forAll gen $ \(x, _, z) -> x < z
  where gen :: Gen (a, a, a)
        gen = suchThat (arbitrary :: Gen (a, a, a)) $ \(x, y, z) -> x < y && y < z

laws :: Constraints a => [(String, Property)]
laws =
    [ ("Precedence Irreflexive", prop_precedenceIrreflexive)
    , ("Precedence Asymmetric", prop_precedenceAsymmetric)
    , ("Precedence Transitive", prop_precedenceTransitive)
    ]
