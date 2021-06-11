module Relation.Order.TotalOrder where

import Relation.Order.PartialOrder

--------------------------------------------

class (PartialOrder a) => TotalOrder a 
