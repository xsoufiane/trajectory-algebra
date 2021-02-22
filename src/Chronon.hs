module Time where

data Chronon

-- | Precedence relation
(<) :: Chronon -> Chronon -> Boolean

-- | Chronon Laws :
forall (t1 :: Chronon) (t2 :: Chronon) (t3 :: Chronon).
  t1 < t2 && t2 < t3 => t1 < t3 -- ^ Transitivity

forall (t :: Chronon).
  not t < t -- ^ Irreflexitivity

forall (t1 :: Chronon) (t2 :: Chronon).
  t1 < t2 => not t2 < t1 -- ^ Asymmetry

forall (t1 :: Chronon) (t2 :: Chronon).
  t1 < t2 || t2 < t1 || t1 == t2 -- ^ Linearity
