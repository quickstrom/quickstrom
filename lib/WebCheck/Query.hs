{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.Query where

import Data.Aeson as JSON
import Data.Aeson.Types (parseFail)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import WebCheck.Element
import GHC.Generics (Generic)

newtype Query = Query 
  deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON Query where
  parseJSON = withObject "Query" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "element" -> ByCss . Selector <$> o .: "selector"
      "elementState" -> Get <$> o .: "stateQuery" <*> o .: "elementQuery"
      _ -> parseFail "Invalid query"

instance ToJSON Query where
  toJSON = \case
    ByCss (Selector selector) ->
      object
        [ "tag" .= ("element" :: Text),
          "selector" .= selector
        ]
    Get state sub ->
      object
        [ "tag" .= ("elementState" :: Text),
          "elementQuery" .= toJSON sub,
          "stateQuery" .= toJSON state
        ]

instance FromJSONKey Query

instance ToJSONKey Query
