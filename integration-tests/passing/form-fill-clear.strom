module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..))
import Data.String (trim)

readyWhen :: String
readyWhen = "body"

actions :: Actions
actions =
  clicks
    <> [ click "input[type=reset]" `weighted` 1 ]
    <> [ click "input[type=submit]" `weighted` 1 ]
    <> [ click "input[type=reset]"
           `followedBy` focus "input[type=email]"
           `followedBy` enterText "jane.doe@example.com"
           `followedBy` focus "input[type=number]"
           `followedBy` enterText "30"
           `weighted` 5
      ]

proposition :: Boolean
proposition =
  let
    initial = emptyForm

    emptyForm = email == Just "" && age == Just ""

    filledForm = email == Just "jane.doe@example.com" && age == Just "30"

    reset = next emptyForm

    fill = emptyForm && next filledForm
  in
    initial && always (reset || fill)

email :: Maybe String
email = map _.value (queryOne "input[type=email]" { value })

age :: Maybe String
age = map _.value (queryOne "input[type=number]" { value })
