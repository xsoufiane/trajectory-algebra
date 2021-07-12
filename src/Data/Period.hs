{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period where
--    ( -- * Data Types
--      Period
--    , SPeriodType(..)
--    , PeriodType(..)
--    , RefinedPeriod 
--       
--      -- * Constructors
--    , period
--    --, periodTH
--    
--      -- * Observations
----    , (===)
----    , PeriodOps(..)
----    , PeriodEqOps(..)
--    ) where
--
--import Control.Exception.Base
--import Data.Data
--import Data.Singletons.TH
--import Language.Haskell.TH.Syntax
--import Refined
--
--import Data.Chronon (Chronon, ChrononOps)
--import qualified Data.Chronon as C (ChrononOps((<)))
--
----import Relation.Identity
--
--import Prelude hiding ((<), (==), (<=))
----import qualified Prelude as Pr ((==))
----
----import Data.Chronon (Chronon)
----
----import Relation.Order.Precedence
----import qualified Relation.Order.Precedence as P ((<))
----
----import Relation.Order.PrecedenceEq
----import qualified Relation.Order.PrecedenceEq as PEq ((<=))
--
-----------------------------------------------------------
--
--data PeriodType = Open | Closed | RightClosed | LeftClosed
--
-- $(genSingletons [''PeriodType])
--
--data Period (c :: PeriodType) t = Period { inf :: Chronon t, sup :: Chronon t }
-- --deriving (Lift, Show)
--
---- | Exceptions
--data InvalidPeriod = InvalidPeriod deriving (Show)
--
--instance Exception InvalidPeriod where
--    displayException _ = "Period Bounds are Invalid!!!" 
--
---- | Refinement
--data ValidPeriodBounds deriving (Typeable)
--
--instance ChrononOps Chronon => Predicate ValidPeriodBounds (Period c t) where
--    validate _ (Period x y)
--        | (C.<) x y = throwRefineSomeException 
--            (typeRep (Proxy :: Proxy ValidPeriodBounds)) 
--            (SomeException InvalidPeriod)
--        | otherwise = Nothing 
--
--type RefinedPeriod c t = Refined ValidPeriodBounds (Period c t)
--
---- | Smart Constructor
--period 
--    :: Predicate ValidPeriodBounds (Period c t)
--    => SPeriodType c 
--    -> Chronon t
--    -> Chronon t
--    -> Either RefineException (RefinedPeriod c t)
--period _ x y = refine $ Period x y

--periodTH
--    :: (PrecedenceEq Chronon, Lift Chronon)
--    => SPeriodType c
--    -> Chronon 
--    -> Chronon
--    -> Q (TExp (RefinedPeriod c))
--periodTH _ x y = refineTH $ Period x y

-- | Observations
--class ChrononOps Chronon => PeriodOps c t where 
--    (<) :: Period c t -> Period c t -> Bool
--    
--    (>) :: Period c t -> Period c t -> Bool
--    x > y = y < x
--    
--    includedIn :: Period c t -> Period c t -> Bool
--    includedIn (Period b c) (Period a d) = (C.<) a b && (C.<) c d

-- 
-- 
--instance Identity Chronon => Identity (Period c) where
--    Period a b === Period a' b' = (===) a a' && (===) b b'
--
--(<) :: PrecedenceEq Chronon => Period 'Open -> Period 'Open -> Bool
--Period _ x < Period y _ = (PEq.<=) x y


---- | Open Periods    
--instance PrecedenceEq t => Precedence (Period 'Open t) where
--    Period _ x < Period y _ = (PEq.<=) x y
    
-- | Open Periods 

--class (Precedence t) => PeriodOps c t where
--    includedIn :: Period c t -> Period c t -> Bool
--    includedIn (Period b c) (Period a d) = (P.<) a b && (P.<) c d
--    
--instance Eq t => Eq (Period c t) where
--    Period a b == Period a' b' = (Pr.==) a a' && (Pr.==) b b'
--   
--class (PrecedenceEq t, PeriodOps c t) => PeriodEqOps c t where
--    (<=) :: Period c t -> Period c t -> Bool
--    x <= y = x < y || (Pr.==) x y
--   
--    includedInOrEq :: Period c t -> Period c t -> Bool
--    includedInOrEq (Period b c) (Period a d) = (PEq.<=) a b && (PEq.<=) c d        
--
-- | Ops Instances
--instance Precedence t => PeriodOps 'Open t where
--  Period _ x < Period y _ = (PEq.<=) x y
--  
--instance PrecedenceEq t => PeriodOps 'Open t where
--  Period _ x < Period y _ = (PEq.<=) x y
--
--instance PrecedenceEq t => PeriodOps 'Closed t where
--  Period _ x < Period y _ = (P.<) x y
--
--instance PrecedenceEq t => PeriodOps 'RightClosed t where
--  Period _ x < Period y _ = (PEq.<=) x y
--
--instance PrecedenceEq t => PeriodOps 'LeftClosed t where
--  Period _ x < Period y _ = (PEq.<=) x y    

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
