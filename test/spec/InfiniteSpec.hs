{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InfiniteSpec where

import Prelude hiding ((<))  
import Data.Proxy (Proxy)
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)
  
--import Begin
--import StrictPartialOrder
--
----------------------------------------------------------
--
--prop_past :: forall m a. (?proxy :: Proxy a, Begin, StrictPartialOrder a, Show a, Serial m a) => Property m
--prop_past = exists (\(x :: a) -> forAll $ \(y :: a) -> not $ y < x) ==> False 
