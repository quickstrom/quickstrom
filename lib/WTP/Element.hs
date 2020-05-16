{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.Element where

import qualified Data.Aeson as JSON
import Data.Hashable (Hashable (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

newtype Element = Element {ref :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable)

instance Pretty Element where
  pretty = pretty . ref

instance JSON.ToJSON Element where
  toJSON (Element e) = JSON.object ["element-6066-11e4-a52e-4f735466cecf" JSON..= e]

newtype Selector = Selector Text
  deriving (Eq, Ord, Show, Generic, Hashable)

instance IsString Selector where
  fromString = Selector . fromString

instance Pretty Selector where
  pretty (Selector s) = pretty s
