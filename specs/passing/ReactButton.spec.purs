module Specification where

import WebCheck
import Data.Maybe (Maybe(..))

readyWhen = ".message"

actions = clicks

proposition :: Boolean
proposition =
  let
    click = messageIs "" && next (messageIs "Hello!")
  in
    messageIs "" && always click

messageIs :: String -> Boolean
messageIs message = queryOne ".message" { textContent } == Just { textContent: message }
