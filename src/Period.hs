{-# LANGUAGE FlexibleContexts #-}

module Period where

import LinearOrder    
    
---------------------------------------  

data Period a = Period { inf :: a, sup :: a}

--instance LinearOrder a => LinearOrder (Period a) where
--  x < y = sup x < inf y
--  x === y = inf x === inf y && sup x === sup y
--
---- | constructors
--period :: LinearOrder (Period a) => a -> a -> Period a
--period = Period
--
---- | observations
----member
