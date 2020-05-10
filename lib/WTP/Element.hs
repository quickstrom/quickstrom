{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WTP.Element where

import qualified Data.Aeson as JSON
import Data.Hashable (Hashable (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Type.Reflection

newtype Element = Element {ref :: Text}
  deriving (Eq, Ord, Show, Hashable, Pretty)

data ElementState a where
  Attribute :: Text -> ElementState Text
  Property :: Text -> ElementState JSON.Value
  CssValue :: Text -> ElementState Text
  Text :: ElementState Text
  Enabled :: ElementState Bool

deriving instance Eq (ElementState a)

deriving instance Show (ElementState a)

instance Hashable (ElementState a) where
  hashWithSalt s = \case
    Attribute t -> s `hashWithSalt` (0 :: Int) `hashWithSalt` t
    Property t -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t
    CssValue t -> s `hashWithSalt` (2 :: Int) `hashWithSalt` t
    Text -> s `hashWithSalt` (3 :: Int)
    Enabled -> s `hashWithSalt` (4 :: Int)

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
