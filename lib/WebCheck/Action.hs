{-# LANGUAGE DeriveGeneric #-}
module WebCheck.Action where

import GHC.Generics (Generic)
import WebCheck.Element
import Text.URI (URI)

data Selected = Selected Selector Int
  deriving (Eq, Show, Generic)

data Action sel = Focus sel | KeyPress Char | Click sel | Navigate URI
  deriving (Eq, Show, Generic)

