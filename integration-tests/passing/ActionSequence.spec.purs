-- This spec only tests that the fixed sequence of actions works, even though
-- the `button` isn't in the DOM until after the form has been submitted.
module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (trim)

readyWhen :: String
readyWhen = "form"

actions :: Actions 
actions = [ 
  focus "input[type=text]" 
    `followedBy` enterText "Hello" 
    `followedBy` specialKeyPress KeyEnter 
    -- `followedBy` awaitWithTimeoutSecs 2 "button"
    `followedBy` click "button" 
  ]

buttonDisabled :: Maybe Boolean
buttonDisabled = _.disabled <$> queryOne "button" { disabled }

proposition :: Boolean
proposition = isJust (queryOne "form" {}) && (buttonDisabled == Nothing)
  `until` (buttonDisabled == Just true)