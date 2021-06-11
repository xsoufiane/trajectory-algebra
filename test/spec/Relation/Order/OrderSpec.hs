{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Relation.Order.OrderSpec (spec) where

import Data.Proxy (Proxy(..))
--import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.QuickCheck as QC
--import Test.Tasty.SmallCheck as SC

import Prelude hiding ((<), (<=))
import qualified Prelude as P ((<), (<=))

--import Finity
import Data.Chronon
import Relation.ChrononImpl
import Relation.Order.CyclicOrder
import Relation.Order.LinearOrder
import Relation.Order.PartialCyclicOrder
import Relation.Order.PartialOrder
import Relation.Order.StrictPartialOrder
import Relation.Order.TotalOrder

--import qualified FinitySpec

import qualified Relation.Order.CyclicOrderSpec as CyclicOrder
import qualified Relation.Order.LinearOrderSpec as LinearOrder
import qualified Relation.Order.PartialCyclicOrderSpec as PartialCyclicOrder
import qualified Relation.Order.PartialOrderSpec as PartialOrder
import qualified Relation.Order.StrictPartialOrderSpec as StrictPartialOrder
import qualified Relation.Order.TotalOrderSpec as TotalOrder

--------------------------------------------------------

-- | Instances
instance StrictPartialOrder (Chronon Int) where
  Chronon x < Chronon y = (P.<) x y    
  
instance LinearOrder (Chronon Int)  

instance PartialOrder (Chronon Int) where
  Chronon x <= Chronon y = (P.<=) x y    
  
instance TotalOrder (Chronon Int)    
  
instance PartialCyclicOrder (Chronon Int) where
    cycle x y z = (x < y && y < z) || (y < z && z < x) || (z < x && x < y)

instance CyclicOrder (Chronon Int)

--instance Begin (Chronon Int)
--instance End (Chronon Int)
--instance UniqueBegin (Chronon Int)
--instance UniqueEnd (Chronon Int)

--instance Monad m => Serial m (Chronon Int) where
--    series = Chronon <$> series

-- | specs
spec :: TestTree
spec = testGroup "Order Spec" [ laws ] --, smallChecks ]

laws :: TestTree
laws = testGroup "Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "LinearOrder" $ LinearOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "PartialOrder" $ PartialOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "TotalOrder" $ TotalOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "PartialCyclicOrder" $ PartialCyclicOrder.laws (Proxy :: Proxy (Chronon Int))
    , QC.testProperties "CyclicOrder" $ CyclicOrder.laws (Proxy :: Proxy (Chronon Int))
    ]

--smallChecks :: TestTree
--smallChecks =
--    testGroup "SmallChecks"
--      [ 
--        testGroup "FinitySpec"
--        [ SC.testProperty 
--            "Begins are not preceded by any other chronon" 
--            $ FinitySpec.prop_begin (Proxy :: Proxy (Chronon Int))
--        , SC.testProperty 
--            "Ends do not precede any other chronon"
--            $ FinitySpec.prop_end (Proxy :: Proxy (Chronon Int))
--        , SC.testProperty 
--            "UniqueBegin Spec"
--            $ FinitySpec.prop_begin (Proxy :: Proxy (Chronon Int))
--        , SC.testProperty 
--            "UniqueEnd Spec"
--            $ FinitySpec.prop_unique_end (Proxy :: Proxy (Chronon Int))                 
--        ]
--      ]
