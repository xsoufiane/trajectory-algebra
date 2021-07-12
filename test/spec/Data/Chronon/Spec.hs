{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Chronon.Spec (spec) where

import Data.Proxy (Proxy(..))  
import Test.QuickCheck hiding ((===))
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((===))

import Prelude hiding ((<), cycle)
import qualified Prelude as P ((<), (==))

import Data.Chronon (Chronon, ChrononOps((<)))
import qualified Data.Chronon as Chronon ((<), (<=), (===), _cycle)

import Test.Relation.Identity (Identity((===)))
import qualified Test.Relation.Identity.Laws as Identity

import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder)
import qualified Test.Relation.Order.StrictPartialOrder (StrictPartialOrder((<)))
import qualified Test.Relation.Order.StrictPartialOrder.Laws as StrictPartialOrder

import Test.Relation.Order.LinearOrder (LinearOrder)
import qualified Test.Relation.Order.LinearOrder.Laws as LinearOrder

import Test.Relation.Order.PartialCyclicOrder (PartialCyclicOrder(cycle))
import qualified Test.Relation.Order.PartialCyclicOrder.Laws as PartialCyclicOrder

import Test.Relation.Order.CyclicOrder (CyclicOrder)
import qualified Test.Relation.Order.CyclicOrder.Laws as CyclicOrder

import Test.Relation.Order.PartialOrder (PartialOrder((<=)))
import qualified Test.Relation.Order.PartialOrder.Laws as PartialOrder

import Test.Relation.Order.TotalOrder (TotalOrder)
import qualified Test.Relation.Order.TotalOrder.Laws as TotalOrder

-------------------------------------

data instance Chronon = Chronon Int deriving (Eq, Show)

instance ChrononOps Chronon where
     Chronon x < Chronon y = (P.<) x y
     Chronon x === Chronon y = (P.==) x y
     _cycle x y z = x < y && y < z || y < z && z < x || z < x && x < y
     
-- | Instances
instance Arbitrary Chronon where
    arbitrary = Chronon <$> arbitrary

instance Identity Chronon where
    (===) = (Chronon.===)

instance StrictPartialOrder Chronon where
    (<) = (Chronon.<)

instance LinearOrder Chronon 

instance PartialCyclicOrder Chronon where
    cycle = Chronon._cycle

instance CyclicOrder Chronon

instance PartialOrder Chronon where
    (<=) = (Chronon.<=)

instance TotalOrder Chronon  

-- | spec
spec :: TestTree
spec = testGroup "Chronon Spec" [ relationLaws, orderLaws ]

relationLaws :: TestTree
relationLaws = testGroup "Relation Laws"
    [ QC.testProperties "Identity" $ Identity.laws (Proxy :: Proxy Chronon)
    ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws (Proxy :: Proxy Chronon)
    , QC.testProperties "LinearOrder" $ LinearOrder.laws (Proxy :: Proxy Chronon)
    , QC.testProperties "PartialCyclicOrder" $ PartialCyclicOrder.laws (Proxy :: Proxy Chronon)
    , QC.testProperties "CyclicOrder" $ CyclicOrder.laws (Proxy :: Proxy Chronon)
    , QC.testProperties "PartialOrder" $ PartialOrder.laws (Proxy :: Proxy Chronon)
    , QC.testProperties "TotalOrder" $ TotalOrder.laws (Proxy :: Proxy Chronon)
    ]
  