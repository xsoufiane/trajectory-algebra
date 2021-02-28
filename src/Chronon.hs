module Time where

data Chronon

-- | Observations
(<) :: Chronon -> Chronon -> Boolean
betweenness :: Chronon -> Chronon -> Chronon -> Boolean
concurrent :: Chronon -> Chronon -> Boolean
synchronous :: Chronon -> Chronon -> Boolean

-- | Precedence Laws :
forall (x :: Chronon).
  not x < x -- ^ Irreflexitivity

forall (x :: Chronon) (y :: Chronon).
  x < y => not x < y -- ^ Asymmetry
  
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x < y && y < z => x < z -- ^ Transitivity

forall (x :: Chronon) (y :: Chronon).
  x < y || y < x || x == y -- ^ Linearity
  
-- | Betweenness  Laws :
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  betweenness(x, y, z) <=> (y < x &&  x < z)  || (z < x && x < y)
  
-- | Concurrency  Laws :
forall (x :: Chronon).
  x `concurrent` x -- ^ reflexitivity

forall (x :: Chronon) (y :: Chronon).
  x `concurrent` y <=> y `concurrent` x  -- ^ Symmetry
  
forall (x :: Chronon) (y :: Chronon).
  x `concurrent` y = not x < y && not y < x
  
-- | Synchronicity  Laws :
forall (x :: Chronon).
  x `synchronous` x -- ^ reflexitivity
  
forall (x :: Chronon) (y :: Chronon).
  x `synchronous` y <=> y `synchronous` x  -- ^ Symmetry
  
forall (x :: Chronon) (y :: Chronon) (z :: Chronon).
  x `synchronous` y && y `synchronous` z => x `synchronous` z -- ^ Transitivity
  
forall (x :: Chronon) (y :: Chronon).
  x `synchronous` y = (forall (t :: Chronon). x < t => y < t) && not t1 < t2 -- ????



