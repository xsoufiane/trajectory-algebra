module Chronon where
  
type Chronon = Int

(===) :: Chronon -> Chronon -> Bool -- ^ Identity
(===) = (==)

betweenness :: Chronon -> Chronon -> Chronon -> Bool
betweenness b a c = a < b && b < c