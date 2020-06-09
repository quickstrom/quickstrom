module ToggleSpecification where

import WebCheck.DSL

origin = "example.com/toggle.html"

readyWhen = "button"

actions = clicks

proposition =
  let on = buttonText == Just "Turn me off"
      off = buttonText == Just "Turn me on"
      turnOn = off && next on
      turnOff = on && next off
  in off && always (turnOn || turnOff)

buttonText = do
    b <- queryOne "button" { text: textContent }
    pure b.text
