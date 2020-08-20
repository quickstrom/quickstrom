{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Quickstrom.Action where

import GHC.Generics (Generic)
import Quickstrom.Element
import Quickstrom.Prelude
import Data.Aeson (ToJSON)

data Selected = Selected Selector Int
  deriving (Eq, Show, Generic, ToJSON)

data Action sel
  = Focus sel
  | KeyPress Char
  | EnterText Text
  | Click sel
  | Navigate Text
  deriving (Eq, Show, Generic, ToJSON)
