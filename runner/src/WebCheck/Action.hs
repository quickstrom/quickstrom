{-# LANGUAGE DeriveGeneric #-}

module WebCheck.Action where

import GHC.Generics (Generic)
import Text.URI (URI)
import WebCheck.Element
import WebCheck.Prelude

data Selected = Selected Selector Int
  deriving (Eq, Show, Generic)

data Action sel
  = Focus sel
  | KeyPress Char
  | EnterText Text
  | Click sel
  | Navigate URI
  deriving (Eq, Show, Generic)
