{-# LANGUAGE DeriveGeneric #-}
module WTP.Specification where

import GHC.Generics (Generic)
import WTP.Core

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Specification formula
  = Specification
      { actions :: [Action]
      , property :: formula
      }
  deriving (Show, Generic)
