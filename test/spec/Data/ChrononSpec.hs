{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ChrononSpec (spec) where

import Data.Proxy (Proxy(..))  
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding ((<))
import qualified Prelude as P ((<))

import Data.Chronon
import Relation.Identity
import Relation.Order.CyclicOrder
import Relation.Order.LinearOrder
import Relation.Order.PartialCyclicOrder
import Relation.Order.PartialOrder
import Relation.Order.StrictPartialOrder
import Relation.Order.TotalOrder

import qualified Relation.IdentitySpec as Identity
import qualified Relation.Order.CyclicOrderSpec as CyclicOrder
import qualified Relation.Order.LinearOrderSpec as LinearOrder
import qualified Relation.Order.PartialCyclicOrderSpec as PartialCyclicOrder
import qualified Relation.Order.PartialOrderSpec as PartialOrder
import qualified Relation.Order.StrictPartialOrderSpec as StrictPartialOrder
import qualified Relation.Order.TotalOrderSpec as TotalOrder

-------------------------------------

data instance Chronon Int = Chronon Int deriving (Eq, Show)

-- | Instances
instance Arbitrary (Chronon Int) where
  arbitrary = Chronon <$> arbitrary

instance Identity (Chronon Int) where
  (===) = (==)

instance StrictPartialOrder (Chronon Int) where
  Chronon x < Chronon y = (P.<) x y    
  
instance LinearOrder (Chronon Int)  

instance PartialOrder (Chronon Int)   
  
instance TotalOrder (Chronon Int)    
  
instance PartialCyclicOrder (Chronon Int) where
    cycle x y z = (x < y && y < z) || (y < z && z < x) || (z < x && x < y)

instance CyclicOrder (Chronon Int)

-- | specs
spec :: TestTree
spec = testGroup "Chronon Spec" [ relationLaws, orderLaws ]

relationLaws :: TestTree
relationLaws = testGroup "Relation Laws"
    [ QC.testProperties "IdentitySpec" $ Identity.laws (Proxy :: Proxy (Chronon Int))
    ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "LinearOrder" $ LinearOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "PartialOrder" $ PartialOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "TotalOrder" $ TotalOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "PartialCyclicOrder" $ PartialCyclicOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "CyclicOrder" $ CyclicOrder.laws (Proxy :: Proxy (Chronon Int))
    ]
  