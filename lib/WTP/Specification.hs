{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module WTP.Specification where

import GHC.Generics (Generic)
import WTP.Query
import Data.Text (Text)
import Data.String (IsString)

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Specification formula
  = Specification
      { actions :: [Action]
      , property :: formula
      }
  deriving (Show, Generic)
