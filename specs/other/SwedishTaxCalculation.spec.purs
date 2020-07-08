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
  , Tuple 1 (Click "app-gdpr-modal #btn-center-confirm")
  , Tuple 1 (Click "[role=main] form a")
  , Tuple 1 (Click "[role=main] form input")
  , Tuple 1 (Click "[role=main] form button")
  , Tuple 5 (Focus "input[type=text]")
  ]

proposition :: Boolean
proposition =
  map _.classes (queryAll "#wizz-lon-efter-skatt li" { classes: attribute (SProxy :: SProxy "class") })
    == [ Just "active", Nothing ]
