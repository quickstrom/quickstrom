{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
-- | A timeout in milliseconds.
module Quickstrom.Timeout where

import Quickstrom.Prelude
import qualified Data.Aeson as JSON

newtype Timeout = Timeout Word64
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

mapTimeout :: (Word64 -> Word64) -> Timeout -> Timeout
mapTimeout f (Timeout ms) = Timeout (f ms)

