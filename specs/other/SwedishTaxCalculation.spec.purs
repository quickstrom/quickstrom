-- webcheck specs/other/SwedishTaxCalculation.spec.purs https://app.skatteverket.se/rakna-skatt-client-skut-skatteutrakning/
module Spec where

import WebCheck
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (SProxy(..))

readyWhen :: Selector
readyWhen = "app-gdpr-modal"

actions :: Actions
actions =
  [ Tuple 1 (Click "[role=main] a")
  , Tuple 5 (Click "app-gdpr-modal #btn-center-confirm")
  , Tuple 5 (Click ".modal-content button")
  , Tuple 2 (Click ".panel-footer button")
  -- targeted form events
  , Tuple 3 (Focus "form input")
  , Tuple 5 (Click "input[type=radio]")
  , Tuple 2 (Click "form select option")
  , Tuple 2 (Click "form .cm-scroll-box input")
  , Tuple 5 (EnterText "1990") -- year
  , Tuple 2 (EnterText "1950") -- year
  , Tuple 5 (EnterText "19900") -- salary (but also a year, the first 4 chars)
  ]

proposition :: Boolean
proposition =
  map _.classes (queryAll "#wizz-lon-efter-skatt li" { classes: attribute (SProxy :: SProxy "class") })
    == [ Just "active", Nothing ]
