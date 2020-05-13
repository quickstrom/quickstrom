{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Prelude hiding (all)
import Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Text (Text)
import WTP.Syntax

inputValue :: Selector -> Formula (Maybe Text)
inputValue sel =
  queryOne (property "value" (byCss sel))
    <&> fmap fromJSON
    <&> ( >>=
            \case
              JSON.Success a -> Just a
              JSON.Error {} -> Nothing
        )

isVisible :: Selector -> Proposition
isVisible sel = (== Just "block") <$> queryOne (cssValue "display" (byCss sel))

hasText :: Selector -> Text -> Proposition
hasText sel message =
  (== Just message) <$> queryOne (text (byCss sel))
