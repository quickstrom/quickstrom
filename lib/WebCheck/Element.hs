{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.Element where

import Data.Aeson as JSON
import Data.Aeson.Types (parseFail)
import Data.Hashable (Hashable (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

newtype Element = Element {ref :: Text}
  deriving (Eq, Ord, Show, Hashable, Pretty)

instance JSON.ToJSON Element where
  toJSON (Element e) = JSON.object ["element-6066-11e4-a52e-4f735466cecf" JSON..= e]

instance JSON.FromJSON Element where
  parseJSON = withObject "Element" $ \o -> do
    e <- o JSON..: "element-6066-11e4-a52e-4f735466cecf"
    pure (Element e)

data ElementState
  = Attribute Text
  | Property Text
  | CssValue Text
  | Text
  | Enabled
  deriving (Eq, Ord, Show, Generic)

instance Hashable ElementState where
  hashWithSalt s = \case
    Attribute t -> s `hashWithSalt` (0 :: Int) `hashWithSalt` t
    Property t -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t
    CssValue t -> s `hashWithSalt` (2 :: Int) `hashWithSalt` t
    Text -> s `hashWithSalt` (3 :: Int)
    Enabled -> s `hashWithSalt` (4 :: Int)

instance JSON.ToJSON ElementState where
  toJSON = \case
    Attribute name -> JSON.object ["tag" .= ("attribute" :: Text), "name" .= name]
    Property name -> JSON.object ["tag" .= ("property" :: Text), "name" .= name]
    CssValue name -> JSON.object ["tag" .= ("cssValue" :: Text), "name" .= name]
    Text -> JSON.object ["tag" .= ("text" :: Text)]
    Enabled -> JSON.object ["tag" .= ("enabled" :: Text)]

instance JSON.FromJSON ElementState where
  parseJSON = withObject "ElementState" $ \o -> do
    tag <- o JSON..: "tag"
    case tag of
      "attribute" -> Attribute <$> (o .: "name")
      "property" -> Property <$> (o .: "name")
      "cssValue" -> CssValue <$> (o .: "name")
      "enabled" -> pure Enabled
      "text" -> pure Text
      t -> parseFail ("Invalid tag: " <> t <> " in object: " <> show o)

newtype Selector = Selector Text
  deriving (Eq, Ord, Show, IsString, Generic, Hashable, Pretty)
