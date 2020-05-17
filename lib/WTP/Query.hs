{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module WTP.Query where

import Data.Aeson as JSON
import Data.Text (Text)
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Element
import Data.Hashable (Hashable (..))

data Query t where
  ByCss :: Selector -> Query Element
  Get :: (Eq a, Show a, Typeable a, Hashable a) => ElementState a -> Query Element -> Query a

deriving instance Show (Query a)
deriving instance Eq (Query a)
deriving instance Ord (Query a)

instance JSON.ToJSON (Query a) where
  toJSON = \case
    ByCss (Selector selector) -> object ["tag" .= ("element" :: Text), "selector" .= selector]
    Get state sub -> object ["elementState" .= toJSON sub, "stateQuery" .= toJSON state]

instance Hashable (Query a) where
  hashWithSalt s = \case
    ByCss selector -> s `hashWithSalt` (0 :: Int) `hashWithSalt` selector
    Get state sub -> s `hashWithSalt` (1 :: Int) `hashWithSalt` state `hashWithSalt` sub

data SomeQuery where
  SomeQuery :: forall a. (Show a, Hashable a, Eq a, Typeable a) => Query a -> SomeQuery

deriving instance Show SomeQuery

instance Eq SomeQuery where
  SomeQuery (q1 :: Query t1) == SomeQuery (q2 :: Query t2)  =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> q1 == q2
      Nothing -> False

instance Ord SomeQuery where
  SomeQuery (q1 :: Query t1) `compare` SomeQuery (q2 :: Query t2)  =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> q1 `compare` q2
      Nothing -> EQ

instance Hashable SomeQuery where
  hashWithSalt s (SomeQuery q) = hashWithSalt s q
