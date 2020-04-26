{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module WTP.Specification where

import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import WTP.Query

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data Selected = Selected Selector Int
  deriving (Show, Generic)

data Action sel = Focus sel | KeyPress Char | Click sel | Navigate Path
  deriving (Show, Generic)

data Specification formula
  = Specification
      { origin :: Path,
        actions :: [(Int, Action Selector)],
        property :: formula
      }
  deriving (Show, Generic)
