{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Core where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import qualified Data.Aeson as JSON
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude hiding (False, True)

newtype Element = Element {ref :: Text}
  deriving (Eq, Show, Hashable)

newtype Selector = Selector Text
  deriving (Eq, Show, IsString, Generic, Hashable)

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data ElementState a where
  Attribute :: Text -> ElementState Text
  Property :: Text -> ElementState JSON.Value
  CssValue :: Text -> ElementState Text
  Text :: ElementState Text
  Enabled :: ElementState Bool

deriving instance Eq (ElementState a)

deriving instance Show (ElementState a)

data Assertion a where
  Equals :: (Show a, Eq a) => a -> Assertion a
  Contains :: Text -> Assertion Text
  Satisfies :: Show a => (a -> Bool) -> Assertion a

instance Show (Assertion a) where
  show = \case
    Equals expected -> "(Equals " <> show expected <> ")"
    Contains t -> "(Contains " <> show t <> ")"
    Satisfies _ -> "(Satisfies _)"

data Query a where
  Query :: Selector -> Query (Maybe Element)
  QueryAll :: Selector -> Query [Element]
  Get :: Typeable a => ElementState a -> Element -> Query a

query :: Member Query effs => Selector -> Eff effs (Maybe Element)
query = send . Query

queryAll :: Member Query effs => Selector -> Eff effs [Element]
queryAll = send . QueryAll

get :: (Member Query effs, Typeable a) => ElementState a -> Element -> Eff effs a
get attr = send . Get attr

data Formula where
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Eff '[Query, Error Text] a -> Assertion a -> Formula

instance Show Formula where
  show = \case
    True -> "True"
    Not p -> "(Not " <> show p <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Until p q -> "(Until " <> show p <> " " <> show q <> ")"
    Assert _ assertion -> "(Assert _ " <> show assertion <> ")"
