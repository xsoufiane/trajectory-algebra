--{-# OPTIONS_GHC -fno-warn-orphans #-}
--
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeFamilies #-}

module Test.Chronon where
  
--import Test.QuickCheck
--
--import Prelude hiding ((<))
--import qualified Prelude as P ((<))
--
--import Chronon
--import LinearOrder
--import Order.PartialCyclic
--import Order.StrictPartialOrder
--
-----------------------------------------------
--
--data instance Chronon Int = Chronon Int deriving (Show)

---- | Chronon Int Instances
--instance Arbitrary (Chronon Int) where
--  arbitrary = Chronon <$> arbitrary

--instance StrictPartialOrder (Chronon Int) where
--  Chronon x < Chronon y = (P.<) x y
--
----instance LinearOrder (Chronon Int) where
----  Chronon x === Chronon y = x == y
--
--instance PartialCyclic (Chronon Int) where
--  cycle x y z = (x < y && y < z) || (y < z && z < x) || (z < x && x < y)
