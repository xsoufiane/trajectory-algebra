{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BeginSpec where

import Prelude hiding ((<))  
import Data.Proxy (Proxy)
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)
  
import Begin
import StrictPartialOrder

--------------------------------------------------------

prop :: forall m a. (?proxy :: Proxy a, Begin, StrictPartialOrder a, Show a, Serial m a) => Property m
prop = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ y < x 
