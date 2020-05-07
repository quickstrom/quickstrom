{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Text (Text)
import WTP.Syntax

inputValue :: Selector -> Formula (Maybe Text)
inputValue sel =
  query (traverse (property "value") =<< one sel)
    <&> fmap fromJSON
    <&> ( >>=
            \case
              JSON.Success a -> Just a
              JSON.Error {} -> Nothing
        )

isVisible :: Selector -> Proposition
isVisible sel = (== Just "block") <$> query (traverse (cssValue "display") =<< one sel)

hasText :: Selector -> Text -> Proposition
hasText sel message =
  (== Just message) <$> query (traverse text =<< one sel)
