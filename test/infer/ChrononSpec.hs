{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChrononSpec (sig) where

import Prelude hiding ((<), (>))
import QuickSpec 

import Chronon
import Test.Chronon ()
import LinearOrder

--------------------------------------------------------------------------

instance (Ord a) => Observe () a (Chronon a) where
  observe _ x = unChronon x
  
sig = bg <> cons
  
cons = signature
  [ con "<" ((<) :: Chronon Int -> Chronon Int -> Bool)
  , con ">" ((>) :: Chronon Int -> Chronon Int -> Bool)
  , con "===" ((===) :: Chronon Int -> Chronon Int -> Bool)
  , con "betweenness" (betweenness :: Chronon Int -> Chronon Int -> Chronon Int -> Bool)
  , monoTypeObserve (Proxy :: Proxy (Chronon Int)) 
  ]
  
bg = background
  [ con "and" ((&&) :: Bool -> Bool -> Bool)
  , con "or" ((||) :: Bool -> Bool -> Bool)
  , con "not" (not :: Bool -> Bool)
  , con "F" False
  , con "T" True
  ]
