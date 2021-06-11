{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module Relation.RelationSpec (spec) where

import Data.Proxy (Proxy(..))
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Relation.ChrononImpl

import qualified Relation.IdentitySpec as Identity
import qualified Relation.Order.OrderSpec as Order

--------------------------------------------------------

-- | specs
spec :: TestTree
spec = testGroup "Relations Spec" [ laws, Order.spec ]

laws :: TestTree
laws = testGroup "Laws"
    [ QC.testProperties "IdentitySpec" $ Identity.laws (Proxy :: Proxy (Chronon Int))
    ]
