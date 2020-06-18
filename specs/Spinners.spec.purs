module SpinnersSpecification where

import WebCheck.DSL

import Data.Array (length)

origin :: String
origin = "file:///home/owi/projects/haskell/webcheck/test/spinners.html"

readyWhen :: String
readyWhen = "body"

actions :: Actions
actions = clicks

proposition :: Boolean
proposition =
    let numberOfActiveSpinners = length (queryAll ".spinner.active" {})
    in numberOfActiveSpinners == 0 && always (numberOfActiveSpinners <= 1)
