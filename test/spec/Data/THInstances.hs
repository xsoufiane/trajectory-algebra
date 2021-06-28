{-# LANGUAGE TemplateHaskell #-}

module Data.THInstances where

import Control.Monad (join)
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax  
import Refined 
import Test.QuickCheck

import Data.Period

---------------------------------

genPeriodType :: [Name] -> Q [Dec]
genPeriodType ls = 
      join <$> sequenceQ 
          ((\n -> let t = reifyType n
                      v =  return $ ConE n
                  in [d|
                         instance Arbitrary $t where
                             arbitrary = return $v
                     |]
          ) <$> ls)
         
genPeriod :: [Name] -> Q [Dec] 
genPeriod ls = 
    join <$> sequenceQ ((\t -> [d|
            instance Arbitrary (Period $t Int) where
                arbitrary = unrefine <$> suchThatMap gen rightToMaybe
                  where
                    gen :: Gen (Either RefineException (RefinedPeriod $t Int))
                    gen = period <$> arbitrary <*> arbitrary <*> arbitrary 
        |]) . return . PromotedT <$> ls)
           