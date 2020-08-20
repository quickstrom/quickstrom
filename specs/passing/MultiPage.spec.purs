module Spec where

import Quickstrom
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Tuple (Tuple(..))

readyWhen :: String
readyWhen = "body"

actions :: Actions
actions =
  clicks
    <> [ Tuple 5 (Focus "input[type=text]") ]
    <> [ Tuple 5 (KeyPress ' '), Tuple 5 (KeyPress 'a') ]

proposition :: Boolean
proposition =
  let
    initial = isJust formMessage

    enterMessage = formMessage /= next formMessage

    submitMessage =
      formMessage /= next submittedMessage
        && isNothing submittedMessage
        && next (isNothing formMessage)
  in
    initial && always (enterMessage || submitMessage)

formMessage :: Maybe String
formMessage = map _.value (queryOne "input" { value })

submittedMessage :: Maybe String
submittedMessage = map _.textContent (queryOne "#message" { textContent })
