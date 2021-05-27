{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndSpec where

import Prelude hiding ((<))  
import Data.Proxy (Proxy)
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)
  
import End
import StrictPartialOrder

--------------------------------------------------------

prop :: forall m a. (?proxy :: Proxy a, End, StrictPartialOrder a, Show a, Serial m a) => Property m
prop = exists $ \(x :: a) -> forAll $ \(y :: a) -> not $ x < y 
