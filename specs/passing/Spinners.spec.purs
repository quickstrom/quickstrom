module SpinnersSpecification where

import WebCheck

import Data.Array (length)

readyWhen :: String
readyWhen = "body"

actions :: Actions
actions = clicks

proposition :: Boolean
proposition =
    let numberOfActiveSpinners = length (queryAll ".spinner.active" {})
    in numberOfActiveSpinners == 0 && always (numberOfActiveSpinners <= 1)
