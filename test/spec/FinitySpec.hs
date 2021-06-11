{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FinitySpec where

import Prelude hiding ((<))  
import Data.Proxy (Proxy(..))
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck as SC

import Finity
--import Order.StrictPartialOrder
import Test.Chronon

-----------------------------------------------------------------------------

--prop_begin :: forall a. (Begin a, Serial IO a, Show a, StrictPartialOrder a) => Proxy a -> Property IO
--prop_begin _ = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ y < x
--
--prop_end :: forall a. (End a, Serial IO a, Show a, StrictPartialOrder a) => Proxy a -> Property IO
--prop_end _ = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ x < y
--
--prop_unique_begin :: forall a. (UniqueBegin a, Serial IO a, Show a) => Proxy a -> Property IO
--prop_unique_begin _ = existsUnique $ \(x :: a) -> forAll $ \(y :: a) -> not $ y < x
--
--prop_unique_end :: forall a. (UniqueEnd a, Serial IO a, Show a) => Proxy a -> Property IO
--prop_unique_end _ = existsUnique $ \(x :: a) -> forAll $ \(y :: a) -> not $ x < y
--
--prop_succession_past :: forall a. (SuccessionPast a, Serial IO a, Show a) => Proxy a -> Property IO
--prop_succession_past _ = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ y < x
--
--prop_succession_future :: forall a. (SuccessionFuture a, Serial IO a, Show a) => Proxy a -> Property IO
--prop_succession_future _ = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ x < y
