{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Relation.ChrononImpl where

import Test.QuickCheck

import Data.Chronon
import Relation.Identity

--------------------------------------------

data instance Chronon Int = Chronon Int deriving (Eq, Show)

-- | Instances
instance Arbitrary (Chronon Int) where
  arbitrary = Chronon <$> arbitrary

instance Identity (Chronon Int) where
  (===) = (==)
