{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChrononSpec where

import Data.Proxy (Proxy(..))
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Test.Chronon
import LinearOrder ()

import qualified BeginSpec
import qualified EndSpec
import qualified LinearOrderSpec
import qualified StrictPartialOrderSpec

--------------------------------------------------------

instance Monad m => Serial m (Chronon Int) where
    series = Chronon <$> series

spec :: TestTree
spec = testGroup "Chronon Int Spec" [ laws, smallChecks ]

laws :: TestTree
laws = testGroup "Laws"
    [ QC.testProperties "StrictPartialOrder" StrictPartialOrderSpec.laws
    , QC.testProperties "LinearOrder" LinearOrderSpec.laws
    ] 
  where ?proxy = Proxy :: Proxy (Chronon Int)  
  
smallChecks :: TestTree
smallChecks = localOption (SmallCheckDepth 100) $ 
    testGroup "SmallChecks"
      [ SC.testProperty "Begins are not preceded by any other chronon" BeginSpec.prop 
      , SC.testProperty "Ends do not precede any other chronon" EndSpec.prop
      ]
  where ?proxy = Proxy :: Proxy (Chronon Int)
