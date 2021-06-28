{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Period 
    ( -- * Data Types
      Period
    , SPeriodType(..)
    , PeriodType(..)
    , RefinedPeriod 
       
      -- * Constructor
    , period
    , periodTH
    ) where

import Data.Singletons.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((<), (<=), (>=))
import Refined

import Relation.Order.StrictPartialOrder as SP
import Relation.Order.PartialOrder as P
import Control.Exception.Base
import Data.Data

---------------------------------------------------------

data PeriodType = Open | Closed | RightClosed | LeftClosed

$(genSingletons [''PeriodType])

data Period (c :: PeriodType) t = Period { inf :: t, sup :: t } deriving (Lift, Show)

-- | Exceptions
data InvalidPeriod = InvalidPeriod deriving (Show)

instance Exception InvalidPeriod where
    displayException _ = "Period Bounds are Invalid!!!" 

-- | Refinement
data ValidPeriodBounds deriving (Typeable)

instance PartialOrder t => Predicate ValidPeriodBounds (Period c t) where
    validate _ (Period x y)
        | x >= y = throwRefineSomeException 
            (typeRep (Proxy :: Proxy ValidPeriodBounds)) 
            (SomeException InvalidPeriod)
        | otherwise = Nothing 

type RefinedPeriod c t = Refined ValidPeriodBounds (Period c t)

-- | Smart Constructor
period 
    :: Predicate ValidPeriodBounds (Period c t)
    => SPeriodType c 
    -> t 
    -> t
    -> Either RefineException (RefinedPeriod c t)
period _ x y = refine $ Period x y

periodTH
    :: (PartialOrder t, Lift t)
    => SPeriodType c
    -> t 
    -> t
    -> Q (TExp (RefinedPeriod c t))
periodTH _ x y = refineTH $ Period x y
    
-- | StrictPartialOrder Instances
instance PartialOrder t => StrictPartialOrder (Period 'Open t) where
  Period _ x < Period y _ = (P.<=) x y

instance PartialOrder t => StrictPartialOrder (Period 'Closed t) where
  Period _ x < Period y _ = (SP.<) x y

instance PartialOrder t => StrictPartialOrder (Period 'RightClosed t) where
  Period _ x < Period y _ = (P.<=) x y

instance PartialOrder t => StrictPartialOrder (Period 'LeftClosed t) where
  Period _ x < Period y _ = (P.<=) x y

---- | PeriodOps
--class PeriodOps (c :: PeriodType) where
--    included :: PartialOrder t => Period c t -> Period c t -> Bool
--    Period x y `included` Period z w = (P.>=) z x && (P.<=) y w
--
--    member :: PartialOrder t => t -> Period c t -> Bool
--
--instance PeriodOps 'Open where
--    member t (Period x y) = (SP.>) t x || (SP.<) t y
--
--instance PeriodOps 'Closed where
--    member t (Period x y) = (P.>=) t x || (P.<=) t y
--
--instance PeriodOps 'RightClosed where
--    member t (Period x y) = (SP.>) t x || (P.<=) t y
--
--instance PeriodOps 'LeftClosed where
--    member t (Period x y) = (P.>=) t x || (SP.<) t y
