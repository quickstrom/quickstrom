module SpinnersSpecification where

import Quickstrom
import Data.Maybe (Maybe(..))

readyWhen :: String
readyWhen = "body"

actions :: Actions
actions = clicks

proposition :: Boolean
proposition =
  let
    on = onButton == Just false && offButton == Just true

    off = offButton == Just false && onButton == Just true

    pressOn = off && next on

    pressOff = on && next off
  in
    off && always (pressOn || pressOff)

offButton :: Maybe Boolean
offButton = map _.disabled (queryOne "[name=off]" { disabled })

onButton :: Maybe Boolean
onButton = map _.disabled (queryOne "[name=on]" { disabled })
