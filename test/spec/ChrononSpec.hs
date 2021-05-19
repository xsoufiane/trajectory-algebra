{-# LANGUAGE ScopedTypeVariables #-}

module ChrononSpec where

import Data.Proxy (Proxy(..))
import Test.QuickCheck.Classes

import Chronon
import Test.Chronon ()
import LinearOrder ()

import qualified StrictPartialOrderSpec
import qualified LinearOrderSpec

--------------------------------------------------------

runSpec :: IO ()
runSpec = lawsCheckMany 
  [ ("Chronon", 
      [ StrictPartialOrderSpec.laws (Proxy :: Proxy (Chronon Int))
      , LinearOrderSpec.laws (Proxy :: Proxy (Chronon Int))
      ]
    )
  ]
