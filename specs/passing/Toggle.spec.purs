module ToggleSpecification where

import WebCheck.DSL
import Data.Maybe (Maybe(..))

readyWhen = "button"

actions = clicks

proposition =
  let on = buttonText == Just "Turn me off"
      off = buttonText == Just "Turn me on"
      turnOn = off && next on
      turnOff = on && next off
  in off && always (turnOn || turnOff)

buttonText = map _.textContent (queryOne "button" { textContent })
