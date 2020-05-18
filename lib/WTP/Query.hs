{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WTP.Query where

import Control.Applicative ((<|>))
import Data.Aeson as JSON
import Data.Aeson.Types (Parser, parseFail)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Element

data Query t where
  ByCss :: Selector -> Query Element
  Get :: (Eq a, Show a, Typeable a, Hashable a) => ElementState a -> Query Element -> Query a

deriving instance Show (Query a)

deriving instance Eq (Query a)

deriving instance Ord (Query a)

instance Hashable (Query a) where
  hashWithSalt s = \case
    ByCss selector -> s `hashWithSalt` (0 :: Int) `hashWithSalt` selector
    Get state sub -> s `hashWithSalt` (1 :: Int) `hashWithSalt` state `hashWithSalt` sub

instance FromJSON (Query Element) where
  parseJSON = withObject "Query" $ \o -> do
    ByCss . Selector <$> o .: "selector"

data SomeQuery where
  SomeQuery :: forall a. (Show a, Hashable a, Eq a, Typeable a) => Query a -> SomeQuery

deriving instance Show SomeQuery

instance Eq SomeQuery where
  SomeQuery (q1 :: Query t1) == SomeQuery (q2 :: Query t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> q1 == q2
      Nothing -> False

instance Ord SomeQuery where
  SomeQuery (q1 :: Query t1) `compare` SomeQuery (q2 :: Query t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> q1 `compare` q2
      Nothing -> EQ

instance Hashable SomeQuery where
  hashWithSalt s (SomeQuery q) = hashWithSalt s q

instance ToJSON SomeQuery where
  toJSON = \case
    SomeQuery (ByCss (Selector selector)) ->
      object
        [ "tag" .= ("element" :: Text),
          "selector" .= selector
        ]
    SomeQuery (Get state sub) ->
      object
        [ "tag" .= ("elementState" :: Text),
          "elementQuery" .= toJSON (SomeQuery sub),
          "stateQuery" .= toJSON state
        ]

instance ToJSONKey SomeQuery

instance FromJSON SomeQuery where
  parseJSON = withObject "SomeQuery" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "element" -> SomeQuery <$> (parseJSON (Object o) :: Parser (Query Element))
      "elementState" -> do
        elementQuery :: Query Element <- parseJSON =<< o .: "elementQuery"
        let q :: (Show a, Eq a, Hashable a, Typeable a) => ElementState a -> SomeQuery
            q state' = SomeQuery (Get state' elementQuery)
        stateQuery <- o .: "stateQuery"
        (q <$> parseJSON @(ElementState Text) stateQuery)
          <|> (q <$> parseJSON @(ElementState Value) stateQuery)
          <|> (q <$> parseJSON @(ElementState Bool) stateQuery)
      _ -> parseFail "Invalid query"

instance FromJSONKey SomeQuery
