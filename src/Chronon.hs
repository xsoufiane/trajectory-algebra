module Time where

data Chronon

-- | Relations
(<) :: Chronon -> Chronon -> Boolean
concurrent :: Chronon -> Chronon -> Boolean
synchronous :: Chronon -> Chronon -> Boolean

-- | Precedence Laws :
forall (t1 :: Chronon) (t2 :: Chronon) (t3 :: Chronon).
  t1 < t2 && t2 < t3 => t1 < t3 -- ^ Transitivity

forall (t :: Chronon).
  not t < t -- ^ Irreflexitivity

forall (t1 :: Chronon) (t2 :: Chronon).
  t1 < t2 => not t2 < t1 -- ^ Asymmetry

forall (t1 :: Chronon) (t2 :: Chronon).
  t1 < t2 || t2 < t1 || t1 == t2 -- ^ Linearity
  
-- | Concurrency, Synchronicity  Laws :
forall (t1 :: Chronon) (t2 :: Chronon).
  t1 `concurrent` t2 = not t1 < t2 && not t2 < t1 -- ^ concurrency
forall (t1 :: Chronon) (t2 :: Chronon).
  t1 `synchronous` t2 = (forall (t :: Chronon). t1 < t => t2 < t) && not t1 < t2 -- ^ Synchronicity
forall (t1 :: Chronon) (t2 :: Chronon).
  t1 `synchronous` t2 = (forall (t :: Chronon). t1 < t => t2 < t) && not t1 < t2 -- ^ Synchronicity



