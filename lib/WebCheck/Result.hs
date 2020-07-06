module WebCheck.Result where

import WebCheck.Prelude

data Result
  = Accepted
  | Rejected
  deriving (Eq, Show)
