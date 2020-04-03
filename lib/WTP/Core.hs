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
import Prelude hiding (Bool (..))

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
    Query selector -> "(Query " <> show selector <> ")"
    QueryAll selector -> "(QueryAll " <> show selector <> ")"
    Require query -> "Require (" <> show query <> ")"
    Get attribute query -> "(Get " <> show attribute <> " " <> show query <> ")"
    Map f query -> "(Map _ " <> show query <> ")"

data Language = Full | Simplified

data Formula (l :: Language) where
  -- Simplified language operators
  True :: Formula l
  Not :: Formula l -> Formula l
  Or :: Formula l -> Formula l -> Formula l
  Until :: Formula l -> Formula l -> Formula l
  Assert :: Show a => Query a -> Assertion a -> Formula l
  -- Full language operators
  False :: Formula Full
  Eventually :: Formula Full -> Formula Full
  Always :: Formula Full -> Formula Full
  And :: Formula Full -> Formula Full -> Formula Full
  Implies :: Formula Full -> Formula Full -> Formula Full
  Equivalent :: Formula Full -> Formula Full -> Formula Full
  Release :: Formula Full -> Formula Full -> Formula Full

deriving instance Show (Formula l)

simplify :: Formula Full -> Formula Simplified
simplify = \case
  -- Derived operators (only present in `Full` language) are simplified
  False -> Not True
  Eventually p -> Until True (simplify p)
  Always p -> Not (simplify (Eventually (Not p)))
  And p q -> Not (Not (simplify p) `Or` Not (simplify q))
  Implies p q -> Not (simplify p) `Or` simplify q
  Equivalent p q -> simplify (p `Implies` q) `Or` simplify (q `Implies` p)
  Release p q -> Not (Not (simplify p) `Until` Not (simplify q))
  -- Simplified language operators are preserved
  True -> True
  Not p -> Not (simplify p)
  Or p q -> Or (simplify p) (simplify q)
  Until p q -> Until (simplify p) (simplify q)
  Assert query assertion -> Assert query assertion

data Property
  = Property
      { actions :: [Action],
        specification :: Formula Full
      }
  deriving (Show, Generic)
