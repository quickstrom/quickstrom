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

data Specification formula
  = Specification
      { origin :: Path,
        readyWhen :: Selector,
        actions :: [(Int, Action Selector)],
        proposition :: formula
      }
  deriving (Show, Generic)
