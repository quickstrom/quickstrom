{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module WTP.Core where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.Char                 (Char)
import           Data.Hashable             (Hashable)
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (False, True)

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
  Get :: Attribute a -> Element -> Query a

query :: Member Query effs => Selector -> Eff effs (Maybe Element)
query = send . Query

queryAll :: Member Query effs => Selector -> Eff effs [Element]
queryAll = send . QueryAll

get :: Member Query effs => Attribute a -> Element -> Eff effs a
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
