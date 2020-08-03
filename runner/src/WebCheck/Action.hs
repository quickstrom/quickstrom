{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WebCheck.Action where

import GHC.Generics (Generic)
import WebCheck.Element
import WebCheck.Prelude
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
