module AsyncUpdateSpec where

import WebCheck
import Data.Maybe (Maybe(..))

readyWhen :: String
readyWhen = "button"

actions :: Actions
actions = clicks

proposition :: Boolean
proposition =
  let
    buttonIsEnabled = queryOne "button" { disabled } == Just { disabled: false }

    disabledLaunchWithMessage msg =
      queryOne ".message" { textContent } == Just { textContent: msg }
        && not buttonIsEnabled

    launch =
      buttonIsEnabled
        && next (disabledLaunchWithMessage "Missiles launched.")

    impactOrNoImpact =
      disabledLaunchWithMessage "Missiles launched."
        && next
            ( disabledLaunchWithMessage "Boom!"
                || disabledLaunchWithMessage "Missiles did not hit target."
            )
  in
    buttonIsEnabled && always (launch || impactOrNoImpact)
