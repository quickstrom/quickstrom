{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module WebCheck.Path where

import Data.Text (Text)
import Data.String (IsString)
import GHC.Generics (Generic)

newtype Path = Path Text
  deriving (Eq, Show, IsString, Generic)
