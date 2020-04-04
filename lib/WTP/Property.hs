{-# LANGUAGE DeriveGeneric #-}
module WTP.Property where

import GHC.Generics (Generic)
import WTP.Core

data Property formula
  = Property
      { actions :: [Action],
        specification :: formula
      }
  deriving (Show, Generic)

