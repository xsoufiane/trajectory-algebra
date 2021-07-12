module Test.Relation.Order.LinearOrder where

import Test.Relation.Identity
import Test.Relation.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => LinearOrder a
