-- The following specifies a audio player, featuring a button that
-- toggles between the paused and playing states. The system under
-- test in this case is `RecordPlayer.html`.
module AudioPlayer where

import Quickstrom
import Data.Maybe (Maybe(..))

-- The specification waits for a DOM element matching a CSS selector
-- before taking any action.
readyWhen :: Selector
readyWhen = ".audio-player"

-- Based on the specified actions, Quickstrom generates click actions
-- for all clickable elements.
actions :: Actions
actions = clicks

-- The proposition describes the correct behavior of the web
-- application.  Here we start in the paused state, and a valid
-- transition is either `play` or `pause`.
proposition :: Boolean
proposition =
  let
    -- When in the `playing` state, the button text is "Pause"
    playing = buttonText == Just "Pause"

    -- When in the `paused` state, the button text is "Play"
    paused = buttonText == Just "Play"

    -- The `play` transition means going from `paused` to `playing`
    play = paused && next playing

    -- The `pause` transition means going from `playing` to `paused`
    pause = playing && next paused

    -- The `tick` transitions happens when we're in `playing`,
    -- changing the time display's text
    tick =
      playing
        && next playing
        && timeDisplayText
        < next timeDisplayText
  in
    -- This last part is the central part of the specification,
    -- describing the initial state and the possible transitions. It
    -- can be read in English as:
    --
    --   Initially, the record player is paused. From that point, one
    --   can either play or pause, or the time can tick while playing,
    --   all indefinitely.
    paused && always (play || pause || tick)

-- This helper definition finds an optional text for the play/pause
-- button.
buttonText :: Maybe String
buttonText = map _.textContent (queryOne ".play-pause" { textContent })

-- This helper definition finds an optional text for the time display.
timeDisplayText :: Maybe String
timeDisplayText = map _.textContent (queryOne ".time-display" { textContent })
