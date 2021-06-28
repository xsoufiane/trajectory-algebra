{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.PeriodSpec (spec) where

import Data.Either.Combinators
import Data.Singletons
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Period
import Data.THInstances
import Relation.Order.StrictPartialOrder

import qualified Relation.Order.StrictPartialOrderSpec as StrictPartialOrder

--------------------------------------------------------------

-- | Instances
$(genPeriodType ['SClosed, 'SOpen, 'SRightClosed, 'SLeftClosed])
$(genPeriod ['Closed, 'Open, 'RightClosed, 'LeftClosed])
        
-- | specs
spec :: TestTree
spec = testGroup "Period Spec" [ closedPeriodLaws, openPeriodLaws, rightClosedPeriodLaws, leftClosedPeriodLaws ]

closedPeriodLaws :: TestTree
closedPeriodLaws = testGroup "Closed Period" [ laws (Proxy :: Proxy (Period 'Closed Int))]

openPeriodLaws :: TestTree
openPeriodLaws = testGroup "Open Period" [ laws (Proxy :: Proxy (Period 'Open Int))]

rightClosedPeriodLaws :: TestTree
rightClosedPeriodLaws = testGroup "Right Closed Period" [ laws (Proxy :: Proxy (Period 'RightClosed Int))]

leftClosedPeriodLaws :: TestTree
leftClosedPeriodLaws = testGroup "Left Closed Period" [ laws (Proxy :: Proxy (Period 'LeftClosed Int))]

laws :: (Arbitrary a, StrictPartialOrder a, Show a) => Proxy a -> TestTree
laws p = testGroup "Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws p
    ]
 