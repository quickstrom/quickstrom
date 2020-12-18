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

readyWhen :: Selector
readyWhen = "app-gdpr-modal #btn-center-confirm"

actions :: Actions
actions =
  [ Tuple 1 (Single $ Click "[role=main] a")
  , Tuple 10000000 (Single $ Click "app-gdpr-modal #btn-center-confirm")
  , Tuple 5 (Single $ Click ".modal-content button")
  , Tuple 2 (Single $ Click ".panel-footer button")
  -- targeted form events
  , Tuple 3 (Focus "form input")
  , Tuple 5 (Single $ Click "input[type=radio]")
  , Tuple 2 (Single $ Click "form select option")
  , Tuple 2 (Single $ Click "form .cm-scroll-box input")
  , Tuple 5 (Single $ EnterText "1990") -- year
  , Tuple 2 (Single $ EnterText "1950") -- year
  , Tuple 5 (Single $ EnterText "19900") -- salary (but also a year, the first 4 chars)
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
        )
  where
  initial = isJust gdprModalTitle

  acceptGdpr = isJust gdprModalTitle && next (isNothing gdprModalTitle)

  selectBirthYear = case birthYear of
    Nothing -> next (isJust birthYear)
    Just current -> next (isJust birthYear && Just current /= birthYear)

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
