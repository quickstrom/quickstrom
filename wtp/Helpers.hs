{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Prelude hiding (all)
import Data.Text (Text)
import WTP.Syntax

inputValue :: Selector -> Formula
inputValue sel = queryOne (property "value" (byCss sel))

isVisible :: Selector -> Formula
isVisible sel = "block" === queryOne (cssValue "display" (byCss sel))

hasText :: Selector -> Text -> Formula
hasText sel message =
  string message === queryOne (text (byCss sel))
