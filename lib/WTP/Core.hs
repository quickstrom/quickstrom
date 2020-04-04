{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Core where

import Data.Char (Char)
import Data.Functor (Functor (..))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (True, False)
import Data.Hashable (Hashable)

newtype Selector = Selector Text
  deriving (Eq, Show, IsString, Generic, Hashable)

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Attribute a where
  InnerHTML :: Attribute Text
  InnerText :: Attribute Text
  ClassList :: Attribute [Text]

deriving instance Show (Attribute a)

data Assertion a where
  Equals :: (Show a, Eq a) => a -> Assertion a
  Contains :: Text -> Assertion Text
  Satisfies :: Show a => (a -> Bool) -> Assertion a

instance Show (Assertion a) where
  show = \case
    Equals expected -> "(Equals " <> show expected <> ")"
    Contains t -> "(Contains " <> show t <> ")"
    Satisfies _ -> "(Satisfies _)"

data Element = Element
  deriving (Eq, Show)

data Query a where
  Query :: Selector -> Query (Maybe Element)
  QueryAll :: Selector -> Query [Element]
  Require :: Query (Maybe a) -> Query a
  Get :: Attribute a -> Query Element -> Query a
  Map :: (a -> b) -> Query a -> Query b

instance Functor Query where
  fmap f (Map g q) = Map (fmap f g) q
  fmap f q = Map f q

instance Show (Query a) where
  show = \case
    Query selector -> "(Query " <> show selector <> ")"
    QueryAll selector -> "(QueryAll " <> show selector <> ")"
    Require query -> "(Require " <> show query <> ")"
    Get attribute query -> "(Get " <> show attribute <> " " <> show query <> ")"
    Map _ query -> "(Map _ " <> show query <> ")"

data Formula where
  -- Simplified language operators
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Query a -> Assertion a -> Formula

deriving instance Show Formula
