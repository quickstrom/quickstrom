module Spec where

import WebCheck
import Data.Maybe (Maybe(..), fromMaybe)

readyWhen = "body"

actions = []

display :: CssValue
display = cssValue "display"

fontSize :: CssValue
fontSize = cssValue ("font" <> "-" <> "size")

proposition :: Boolean
proposition = queryOne "p" { display, fontSize } == Just { display: "none", fontSize: "bar" }
