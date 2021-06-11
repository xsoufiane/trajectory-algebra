module Relation.Order.LinearOrder where

import Relation.Identity
import Relation.Order.StrictPartialOrder

--------------------------------------------

class (Identity a, StrictPartialOrder a) => LinearOrder a 
