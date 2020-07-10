-- webcheck specs/other/SwedishTaxCalculation.spec.purs https://app.skatteverket.se/rakna-skatt-client-skut-skatteutrakning/lon-efter-skattetabell/fyll-i-dina-uppgifter
module Spec where

import WebCheck

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..))

readyWhen :: Selector
readyWhen = "app-root"

actions :: Actions
actions =
  [ Tuple 1 (Click "[role=main] a")
  , Tuple 10000000 (Click "app-gdpr-modal #btn-center-confirm")
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
proposition = initial && always (acceptGdpr || selectKommun)
  where
  initial = loading

  loading =
    (_.textContent <$> queryOne "app-root p" { textContent })
      == Just "Nu laddar vi din applikation!"

  acceptGdpr = openModal == Just "Information" && next (openModal == Nothing)

  selectKommun = isNothing selectedKommun && next (isJust selectedKommun)

activeWizardTab :: Maybe String
activeWizardTab = _.textContent <$> queryOne "#wizz-lon-efter-skatt li.active" { textContent }

selectedKommun :: Maybe String
selectedKommun = _.value <$> queryOne "#kommun" { value }

openModal :: Maybe String
openModal = _.textContent <$> queryOne "app-gdpr-modal .modal-title" { textContent }
