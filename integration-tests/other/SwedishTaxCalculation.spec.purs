-- https://app.skatteverket.se/rakna-skatt-client-skut-skatteutrakning/lon-efter-skattetabell/fyll-i-dina-uppgifter
module Spec where

import Quickstrom
import Control.MonadZero (guard)
import Data.Array (null)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (trim, split, replace, Pattern (..), Replacement(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (SProxy(..))

readyWhen :: Selector
readyWhen = "app-gdpr-modal"

actions :: Actions
actions =
  [ (click "app-gdpr-modal #btn-center-confirm") `weighted` 10000000 ]
  <>

  [ click "[role=main] a" `weighted` 1
  , click ".modal-content #btn-abort" `weighted` 5
  , click ".modal-content #btn-close" `weighted` 5
  -- , click ".panel-footer button" `weighted` 2

  , click "input[type=radio]" `weighted` 5
  , click "form select option" `weighted` 2
  , click "form .cm-scroll-box input" `weighted` 2

  -- targeted form actions

  , click "#next" `weighted` 5

  , clear "#fodelsear" `followedBy` focus "#fodelsear" `followedBy` enterText "1950" `weighted` 3
  , clear "#fodelsear" `followedBy` focus "#fodelsear" `followedBy` enterText "1990" `weighted` 3
  , clear "#fodelsear" `followedBy` focus "#fodelsear" `followedBy` enterText "2020" `weighted` 3

  , clear "#inkomst" `weighted` 1
  , focus "#inkomst" `followedBy` enterText "0" `weighted` 5
  , focus "#inkomst" `followedBy` enterText "1" `weighted` 5

  , click "#beraknaknapp" `followedBy` await "#beraknaknapp:not([disabled])" `weighted` 3
  ]

proposition :: Boolean
proposition =
  initial
    && always (noErrorsOnNext && consistentResult)
  where
  initial = isJust gdprModalTitle

  noErrorsOnNext =
    (activeWizardTab == Just "Fyll i dina uppgifter" && next (activeWizardTab == Just "Fyll i din lön"))
      `implies` null errors

  consistentResult =
    case lonResult of
      Just r -> 
        case r.manadsloen of
          Just m -> 
            r.loenefterskatt == m - r.skatt
          Nothing ->
            r.manadsloen == Nothing
              && r.skatt == 0
              && r.loenefterskatt == 0
      Nothing -> true

gdprModalTitle :: Maybe String
gdprModalTitle = _.textContent <$> queryOne "app-gdpr-modal .modal-title" { textContent }

activeWizardTab :: Maybe String
activeWizardTab = do
  tab <- queryOne "#wizz-lon-efter-skatt li.active .content-pane" { textContent }
  pure (trim tab.textContent)

errors :: Array String
errors = (trim <<< _.textContent) <$> (queryAll "app-form-inline-error p" { textContent })

type Result = { manadsloen :: Maybe Int, skatt :: Int, loenefterskatt :: Int }

parseMoney :: String -> Maybe Int
parseMoney = Int.fromString <<< replace (Pattern " ") (Replacement "")

lonResult :: Maybe Result
lonResult = do
  manadsloen <- _.textContent <$> queryOne "#manadsloen" { textContent }
  skatt <- (parseMoney <<< _.textContent) =<< queryOne "#skatt" { textContent }
  loenefterskatt <- (parseMoney <<< _.textContent) =<< queryOne "#loenefterskatt" { textContent }
  pure { manadsloen: parseMoney manadsloen, skatt, loenefterskatt }