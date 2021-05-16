{-# LANGUAGE ScopedTypeVariables #-}

module Test.Chronon where

import Test.QuickCheck

import Chronon
import LinearOrder

---------------------------------------------

instance (Arbitrary a, LinearOrder a) => Arbitrary (Chronon a) where
  arbitrary = chronon <$> arbitrary
