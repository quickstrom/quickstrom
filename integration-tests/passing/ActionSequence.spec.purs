-- This spec only tests that the fixed sequence of actions works, even though
-- the `button` isn't in the DOM until after the form has been submitted.
module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (trim)

readyWhen :: String
readyWhen = "body"

actions :: Actions actions = [ focus "input[type=text]" `followedBy`
enterText "Hello" `followedBy` specialKeyPress KeyEnter `followedBy` click
"button" `weighted` 5 ]

proposition :: Boolean
proposition = true