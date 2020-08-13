{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module WebCheck.Action where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import WebCheck.Element
import WebCheck.Prelude

data Selected = Selected {selector :: Selector, index :: Int}
  deriving (Eq, Show, Generic, ToJSON)

data Action sel
  = Focus {selector :: sel}
  | KeyPress {key :: Char}
  | EnterText {text :: Text}
  | Click {selector :: sel}
  | Navigate {uri :: Text}
  deriving (Eq, Show, Generic, ToJSON)
