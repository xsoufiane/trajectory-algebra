module Relation.Order.CyclicOrder where

import Relation.Identity
import Relation.Order.PartialCyclicOrder

-------------------------

class (PartialCyclicOrder a, Identity a) => CyclicOrder a
  