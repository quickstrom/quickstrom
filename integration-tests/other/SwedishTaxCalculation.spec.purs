-- quickstrom specs/other/SwedishTaxCalculation.spec.purs https://app.skatteverket.se/rakna-skatt-client-skut-skatteutrakning/lon-efter-skattetabell/fyll-i-dina-uppgifter
module Spec where

import Quickstrom
import Control.MonadZero (guard)
import Data.Array (null)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (trim)
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

  , focus "#fodelsear" `followedBy` clear "#fodelsear" `followedBy` enterText "1950" `weighted` 3
  , focus "#fodelsear" `followedBy` clear "#fodelsear" `followedBy` enterText "1990" `weighted` 3
  , focus "#fodelsear" `followedBy` clear "#fodelsear" `followedBy` enterText "2020" `weighted` 3

  , clear "#inkomst" `weighted` 1
  , focus "#inkomst" `followedBy` enterText "0" `weighted` 5
  , focus "#inkomst" `followedBy` enterText "1" `weighted` 5

  , click "#beraknaknapp" `weighted` 3
  ]

proposition :: Boolean
proposition =
  initial
    && always
        ( acceptGdpr
            || selectBirthYear
            || selectKommun
            || selectMemberInKyrkaOrSamfund
            || continueToInkomst
            || submitWithErrors
            || toggleAccordion
        )
  where
  initial = isJust gdprModalTitle

  acceptGdpr = isJust gdprModalTitle && next (isNothing gdprModalTitle)

  selectBirthYear = birthYear /= next birthYear

  selectKommun = case kommun of
    Nothing -> next (isJust kommun)
    Just current -> next (isJust kommun && Just current /= kommun)

  selectMemberInKyrkaOrSamfund = case selectedMemberInKyrkaOrSamfund of
    Nothing -> next (isJust selectedMemberInKyrkaOrSamfund)
    Just Ja -> next (selectedMemberInKyrkaOrSamfund == Just Nej)
    Just Nej -> next (selectedMemberInKyrkaOrSamfund == Just Ja)

  continueToInkomst =
    null errors
      && activeWizardTab
      == Just "Fyll i dina uppgifter"
      && next (activeWizardTab == Just "Fyll i din lön")

  submitWithErrors =
    next (not (null errors))
      && ( activeWizardTab
            == next activeWizardTab
        )
  
  toggleAccordion =
    accordionExpanded /= next accordionExpanded

gdprModalTitle :: Maybe String
gdprModalTitle = _.textContent <$> queryOne "app-gdpr-modal .modal-title" { textContent }

activeWizardTab :: Maybe String
activeWizardTab = do
  tab <- queryOne "#wizz-lon-efter-skatt li.active" { textContent }
  pure (trim tab.textContent)

birthYear :: Maybe Int
birthYear = Int.fromString =<< (_.value <$> queryOne "#fodelsear" { value })

kommun :: Maybe String
kommun = do
  v <- _.value <$> queryOne "#kommun" { value }
  guard (v /= "null")
  pure v

errors :: Array String
errors = (trim <<< _.textContent) <$> (queryAll "app-form-inline-error p" { textContent })

data MemberInKyrkaOrSamfund
  = Nej
  | Ja -- TODO: which one?

derive instance eqMemberInKyrkaOrSamfund :: Eq MemberInKyrkaOrSamfund

selectedMemberInKyrkaOrSamfund :: Maybe MemberInKyrkaOrSamfund
selectedMemberInKyrkaOrSamfund = case _.head.value <$> Array.uncons (Array.filter _.checked memberInKyrkaOrSamfundRadioButtons) of
  Just "Nej" -> pure Nej
  Just "Ja" -> pure Ja
  _ -> Nothing
  where
  memberInKyrkaOrSamfundRadioButtons :: Array { checked :: Boolean, value :: String }
  memberInKyrkaOrSamfundRadioButtons = queryAll "[name=isMemberInKyrkaOrSamfund]" { value, checked }

ariaExpanded :: Attribute "aria-expanded"
ariaExpanded = attribute SProxy

accordionExpanded :: Maybe Boolean
accordionExpanded = do
  expanded <- _.ariaExpanded =<< queryOne "#accordion-lon-efter-skatt .accordion" { ariaExpanded }
  pure (expanded == "true")
