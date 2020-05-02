{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module WTP.Specification where

import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import WTP.Element

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data Selected = Selected Selector Int
  deriving (Show, Generic)

data Action sel = Focus sel | KeyPress Char | Click sel | Navigate Path
  deriving (Show, Generic)

type ActionSequence sel = [Action sel]

data Specification formula
  = Specification
      { origin :: Path,
        actions :: [(Int, ActionSequence Selector)],
        property :: formula
      }
  deriving (Show, Generic)
