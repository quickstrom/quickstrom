
module WTP.Type (Type(..)) where

data Type 
  = Bool 
  | String
  | Element
  | Set Type
  | Seq Type
