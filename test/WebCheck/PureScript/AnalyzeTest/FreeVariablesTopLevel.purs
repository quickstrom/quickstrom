module Spec where

import WebCheck
import Data.Maybe (Maybe(..))

readyWhen = "body"

actions = []

proposition :: Boolean
proposition =
  queryOne sel { display: cssValue "display" } == Just { display: "none" }

sel :: Selector
sel = "p"
