module Relation.Identity where

  
class Identity a where
  (===) :: a -> a -> Bool    
