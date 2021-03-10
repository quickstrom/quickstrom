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
    <> [ Click "input[type=reset]" `weighted` 1 ]
    <> [ Click "input[type=submit]" `weighted` 1 ]
    <> [ ( Click "input[type=reset]"
            :| [ Focus "input[type=email]"
              , EnterText "jane.doe@example.com"
              , Focus "input[type=number]"
              , EnterText "30"
              ]
        )
          `weigthed`
            5
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
