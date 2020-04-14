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

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Specification formula
  = Specification
      { actions :: [Action],
        property :: formula
      }
  deriving (Show, Generic)