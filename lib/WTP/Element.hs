{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WTP.Element where

import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Type.Reflection
import qualified WTP.Type as WTP

newtype Element = Element {ref :: Text}
  deriving (Eq, Ord, Show, Hashable, Pretty)

data ElementState (a :: WTP.Type) where
  Attribute :: Text -> ElementState 'WTP.String
  Property :: Text -> ElementState 'WTP.Json
  CssValue :: Text -> ElementState 'WTP.String
  Text :: ElementState 'WTP.String
  Enabled :: ElementState 'WTP.Bool

deriving instance Eq (ElementState a)

deriving instance Show (ElementState a)

data SomeElementState where
  SomeElementState :: Typeable a => ElementState a -> SomeElementState

instance Eq SomeElementState where
  (SomeElementState (a1 :: ElementState s1)) == (SomeElementState (a2 :: ElementState s2)) =
    case eqTypeRep (typeRep @s1) (typeRep @s2) of
      Just HRefl -> a1 == a2
      Nothing -> False

newtype Selector = Selector Text
  deriving (Eq, Ord, Show, IsString, Generic, Hashable, Pretty)
