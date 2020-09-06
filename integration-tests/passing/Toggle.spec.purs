module ToggleSpecification where

import Quickstrom
import Data.Maybe (Maybe(..))

readyWhen = "button"

actions = clicks

proposition :: Boolean
proposition =
  let
    on = buttonText == Just "Turn me off"

    off = buttonText == Just "Turn me on"

    turnOn = off && next on

    turnOff = on && next off
  in
    off && always (turnOn || turnOff)

buttonText :: Maybe String
buttonText = map _.textContent (queryOne "button" { textContent })
