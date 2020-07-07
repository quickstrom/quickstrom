-- webcheck specs/other/SwedishTaxCalculation.spec.purs https://app.skatteverket.se/rakna-skatt-client-skut-skatteutrakning/
module Spec where

import WebCheck

import Data.Tuple (Tuple(..))

readyWhen :: Selector
readyWhen = "app-root"

actions :: Actions
actions = appFoci <> appClicks
  where
    appClicks = 
      [ Tuple 5 (Click "[role=main] a")
      ]
    appFoci = [ Tuple 5 (Focus "input[type=text]") ]

proposition = true