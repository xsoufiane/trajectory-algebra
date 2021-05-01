{-# LANGUAGE TemplateHaskell #-}
module ChrononSpec where

import Test.QuickCheck hiding ((===))
import Chronon
  

prop_precedenceIrreflexive :: Chronon -> Bool
prop_precedenceIrreflexive x = not $ x < x

prop_precedenceAsymmetric :: Property
prop_precedenceAsymmetric = 
  forAll (suchThat (arbitrary :: Gen (Chronon, Chronon)) $ uncurry (<)) $ \(x, y) -> not $ y < x

prop_precedenceTransitive :: Property
prop_precedenceTransitive = 
  forAll (suchThat (arbitrary :: Gen (Chronon, Chronon, Chronon) ) $ \(x, y , z) -> x < y && y < z) $ \(x, _, z) -> x < z 

prop_precedenceLinearity :: Chronon -> Chronon -> Bool
prop_precedenceLinearity x y = x < y || x > y || x === y


return []
runSpec :: IO Bool
runSpec = $quickCheckAll