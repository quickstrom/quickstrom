{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WTP.Core.ElementState where

import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data ElementState
  = Attribute Text
  | Property Text
  | CssValue Text
  | Text
  | Enabled
  deriving (Eq, Show, Generic, Hashable)
