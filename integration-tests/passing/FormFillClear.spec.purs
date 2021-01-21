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
    <> [ Tuple 1 (Single $ Click "input[type=reset]") ]
    <> [ Tuple 1 (Single $ Click "input[type=submit]") ]
    <> [ Tuple 5
          ( Sequence
              [ Click "input[type=reset]"
              , Focus "input[type=email]"
              , EnterText "jane.doe@example.com"
              , Focus "input[type=number]"
              , EnterText "30"
              ]
          )
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
