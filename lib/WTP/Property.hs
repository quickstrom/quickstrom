{-# LANGUAGE DeriveGeneric #-}
module WTP.Property where

import GHC.Generics (Generic)
import WTP.Core

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Property formula
  = Property
      { actions :: [Action],
        specification :: formula
      }
  deriving (Show, Generic)
