module Period where

data Period

-- | Precedence relation
(<) :: Period -> Period -> Boolean

-- | Inclusion
(in) :: Period -> Period -> Boolean

-- | Overlap
(overlap) :: Period -> Period -> Boolean

-- | Precedence Laws :
forall (t1 :: Period) (t2 :: Period) (t3 :: Period).
  t1 < t2 && t2 < t3 => t1 < t3 -- ^ Transitivity

forall (t :: Period).
  not t < t -- ^ Irreflexitivity

forall (t1 :: Period) (t2 :: Period).
  t1 < t2 => not t2 < t1 -- ^ Asymmetry

forall (t1 :: Period) (t2 :: Period).
  t1 < t2 || t2 < t1 || t1 == t2 -- ^ Linearity

-- | Inclusion Laws :
forall (t1 :: Period) (t2 :: Period) (t3 :: Period).
  t1 in t2 && t2 in t3 => t1 in t3 -- ^ Transitivity

forall (t :: Period).
  t in t -- ^ Reflexitivity

forall (t1 :: Period) (t2 :: Period).
  t1 in t2 => not t2 in t1 -- ^ Asymmetry

-- | Overlap Laws :
forall (t1 :: Period) (t2 :: Period).
 t1 overlap t2 = (forall (t :: Period). t in t1 && t in t2)

forall (t1 :: Period) (t2 :: Period)
  t1 in t2 => forall (t in t1) t overlap t2

forall (t1 :: Period) (t2 :: Period).
 not t1 in t2 => (forall (t in t1). not t in t2) -- ^ non-inclusion, disjoint subperiod

-- | Mixed Laws :
forall (t1 :: Period) (t2:: Period)
  t1 < t2 => forall (t in t2). t < t2 -- ^ Left Monotonicity

forall (t1 :: Period) (t2:: Period)
  t1 < t2 => forall (t in t2). t < t2 -- ^ Right Monotonicity
