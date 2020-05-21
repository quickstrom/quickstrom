{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WTP.Element where

import           Data.Aeson as JSON
import Data.Hashable (Hashable (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Type.Reflection
import Data.Aeson.Types (parseFail)

newtype Element = Element {ref :: Text}
  deriving (Eq, Ord, Show, Hashable, Pretty)

instance JSON.ToJSON Element where
  toJSON (Element e) = JSON.object ["element-6066-11e4-a52e-4f735466cecf" JSON..= e]

instance JSON.FromJSON Element where
  parseJSON = withObject "Element" $ \o -> do
    e <- o JSON..: "element-6066-11e4-a52e-4f735466cecf"
    pure (Element e)

data PropertyValue = PropertyValue { propertyValue :: JSON.Value }
  deriving (Eq)

instance Show PropertyValue where
  show = show . propertyValue

instance JSON.FromJSON PropertyValue where
  parseJSON = withObject "PropertyValue" $ \o -> do
    tag <- o JSON..: "tag"
    case tag of
      "propertyValue" -> PropertyValue <$> (o .: "value")
      _ -> parseFail ("Invalid tag: " <> tag)

instance JSON.ToJSON PropertyValue where
  toJSON = \case
    PropertyValue v -> JSON.object ["tag" .= ("propertyValue" :: Text), "value" .= v]

instance Hashable PropertyValue where
  hashWithSalt s (PropertyValue v) = s `hashWithSalt` v

data ElementState a where
  Attribute :: Text -> ElementState Text
  Property :: Text -> ElementState PropertyValue
  CssValue :: Text -> ElementState Text
  Text :: ElementState Text
  Enabled :: ElementState Bool

deriving instance Eq (ElementState a)
deriving instance Show (ElementState a)
deriving instance Ord (ElementState a)

instance Hashable (ElementState a) where
  hashWithSalt s = \case
    Attribute t -> s `hashWithSalt` (0 :: Int) `hashWithSalt` t
    Property t -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t
    CssValue t -> s `hashWithSalt` (2 :: Int) `hashWithSalt` t
    Text -> s `hashWithSalt` (3 :: Int)
    Enabled -> s `hashWithSalt` (4 :: Int)

instance JSON.ToJSON (ElementState a) where
  toJSON = \case
    Attribute name -> JSON.object ["tag" .= ("attribute" :: Text), "name" .= name]
    Property name -> JSON.object ["tag" .= ("property" :: Text), "name" .= name]
    CssValue name -> JSON.object ["tag" .= ("cssValue" :: Text), "name" .= name]
    Text -> JSON.object ["tag" .= ("text" :: Text)]
    Enabled -> JSON.object ["tag" .= ("enabled" :: Text)]

instance JSON.FromJSON (ElementState Text) where
  parseJSON = withObject "ElementState" $ \o -> do
    tag <- o JSON..: "tag"
    case tag of
      "attribute" -> Attribute <$> (o .: "name")
      "cssValue" -> CssValue <$> (o .: "name")
      "text" -> pure Text
      t -> parseFail ("Invalid tag: " <> t)

instance JSON.FromJSON (ElementState PropertyValue) where
  parseJSON = withObject "ElementState" $ \o -> do
    tag <- o JSON..: "tag"
    case tag of
      "property" -> Property <$> (o .: "name")
      t -> parseFail ("Invalid tag: " <> t)

instance JSON.FromJSON (ElementState Bool) where
  parseJSON = withObject "ElementState" $ \o -> do
    tag <- o JSON..: "tag"
    case tag of
      "enabled" -> pure Enabled
      t -> parseFail ("Invalid tag: " <> t)

data SomeElementState where
  SomeElementState :: (Typeable a, Eq a, Show a) => ElementState a -> SomeElementState

instance Eq SomeElementState where
  (SomeElementState (a1 :: ElementState s1)) == (SomeElementState (a2 :: ElementState s2)) =
    case eqTypeRep (typeRep @s1) (typeRep @s2) of
      Just HRefl -> a1 == a2
      Nothing -> False

deriving instance Show SomeElementState

instance Hashable SomeElementState where
  hashWithSalt s (SomeElementState state) = s `hashWithSalt` state

newtype Selector = Selector Text
  deriving (Eq, Ord, Show, IsString, Generic, Hashable, Pretty)
