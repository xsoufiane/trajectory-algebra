{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Period where

import Data.Singletons.TH
import Prelude hiding ((<), (<=))

import Relation.Order.PartialOrder as P

---------------------------------------------------------

data PeriodType = Open | Closed | RightClosed | LeftClosed

$(genSingletons [''PeriodType])

data Period (c :: PeriodType) t = Period { inf :: t, sup :: t } deriving (Show)

period :: SPeriodType c -> t -> t -> Period c t
period _ = Period

class PeriodOps (c :: PeriodType) where
  (<) :: PartialOrder t => Period c t -> Period c t -> Bool
  member :: PartialOrder t => t -> Period c t -> Bool

instance PeriodOps 'Closed where
  Period _ x < Period y _ = (P.<) x y
  member t (Period x y) = (P.>=) t x || (P.<=) t y
