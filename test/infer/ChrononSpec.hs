{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChrononSpec (sig) where

import Prelude hiding ((<), (>))
import QuickSpec 

import Test.Chronon
import LinearOrder
import StrictPartialOrder

--------------------------------------------------------------------------

instance Observe () Int (Chronon Int) where
  observe _ (Chronon x) = x
  
sig = bg <> cons
  
cons = signature
  [ con "<" ((<) :: Chronon Int -> Chronon Int -> Bool)
  , con ">" ((>) :: Chronon Int -> Chronon Int -> Bool)
  , con "===" ((===) :: Chronon Int -> Chronon Int -> Bool)
  , monoTypeObserve (Proxy :: Proxy (Chronon Int)) 
  ]
  
bg = background
  [ con "and" ((&&) :: Bool -> Bool -> Bool)
  , con "or" ((||) :: Bool -> Bool -> Bool)
  , con "not" (not :: Bool -> Bool)
  , con "F" False
  , con "T" True
  ]
