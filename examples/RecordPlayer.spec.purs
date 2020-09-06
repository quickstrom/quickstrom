module ToggleSpecification where

import Quickstrom
import Data.Maybe (Maybe(..))

readyWhen :: Selector
readyWhen = ".record-player"

actions :: Actions
actions = clicks

proposition :: Boolean
proposition =
  let
    playing = buttonText == Just "Pause"

    paused = buttonText == Just "Play"

    play = paused && next playing

    pause = playing && next paused
  in
    paused && always (play || pause)

buttonText :: Maybe String
buttonText = map _.textContent (queryOne ".play-pause" { textContent })
