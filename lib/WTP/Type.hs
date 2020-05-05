
module WTP.Type (Type(..)) where

data Type 
  = Bool 
  | String
  | Element
  | Json
  | Set Type
  | Seq Type
