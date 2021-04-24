module Chronon where

data Chronon

-- | Observations
(<) :: Chronon -> Chronon -> Boolean -- ^ Precedence

(>) :: Chronon -> Chronon -> Boolean -- ^ After

(===) :: Chronon -> Chronon -> Boolean -- ^ Identity

synchronous :: Chronon -> Chronon -> Boolean

betweenness :: Chronon -> Chronon -> Chronon -> Boolean

-- | Precedence Laws :
forall (x :: Chronon).
  not x < x -- ^ Irreflexitivity

forall (x :: Chronon) (y :: Chronon).
  x < y => not y < x -- ^ Asymmetry

forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x < y && y < z => x < z -- ^ Transitivity

forall (x :: Chronon) (y :: Chronon).
  x < y || x > y || x === y -- ^ Linearity
  
forall (x :: Chronon) (y :: Chronon)
  x < y = not x > y && not x === y
  
-- | Identity Laws :
forall (x :: Chronon).
  x === x -- ^ reflexitivity

forall (x :: Chronon) (y :: Chronon).
  x === y => y === x -- ^ Symmetry
  
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x === y && y === z => x === z -- ^ Transitivity
  
forall (x :: Chronon) (y :: Chronon).
  x === y => not x < y && not x > y
  
-- | After Operator Laws :
forall (x :: Chronon).
  not x > x -- ^ Irreflexitivity

forall (x :: Chronon) (y :: Chronon).
  x > y => not y > x -- ^ Asymmetry

forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x > y && y > z => x > z -- ^ Transitivity

forall (x :: Chronon) (y :: Chronon).
  x > y = not x < y && not x === y

-- | Synchronicity  Laws :
forall (x :: Chronon) (y :: Chronon) (t :: Chronon).
  x `synchronous` y = (x < t => y < t) && not x < y -- ^ Definition
  
forall (x :: Chronon).
  x `synchronous` x -- ^ reflexitivity
  
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x `synchronous` y && y `synchronous` z => x `synchronous` z -- ^ Transitivity

forall (x :: Chronon) (y :: Chronon).
  x > y => not x `synchronous` y
  
forall (x :: Chronon) (y :: Chronon).
  x === y => x `synchronous` y
  
-- | Betweenness  Laws :
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  betweenness x y z = (y < x &&  x < z)  || (z < x && x < y)
  