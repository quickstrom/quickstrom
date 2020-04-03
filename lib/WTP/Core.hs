{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Core where

import Data.Char (Char)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ((<>), ($), (.), Eq, Maybe, Show(..))
import Data.Functor (Functor(..))

newtype Selector = Selector Text
  deriving (Show, IsString, Generic)

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
  Equals :: Eq a => a -> Assertion a
  Contains :: Text -> Assertion Text

deriving instance Show a => Show (Assertion a)

data Element = Element

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
        Query selector -> "Query " <> show selector
        QueryAll selector -> "QueryAll " <> show selector
        Require query -> "Require " <> show query
        Get attribute query -> "Get " <> show attribute <> " " <> show query
        Map f query -> "Map _ " <> show query

data Formula where
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Show a => Query a -> Assertion a -> Formula

deriving instance Show Formula

data Specification
  = Specification
      { actions :: [Action],
        correctness :: Formula
      }
  deriving (Show, Generic)
