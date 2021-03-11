{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.TraceTest where

import Control.Lens
import Data.Generics.Labels ()
import qualified Quickstrom.Gen as Gen
import Quickstrom.Prelude
import Quickstrom.Trace
import Test.QuickCheck (Property, forAll, (===), (==>))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Modifiers (Positive (Positive))
import Test.Tasty.QuickCheck (NonNegative (NonNegative))

prop_all_empty_are_stuttering :: Property
prop_all_empty_are_stuttering = forAll Gen.trace $ \t -> do
  let t' = t & observedStates .~ mempty
  (t' ^. observedStates . #elementStates) === (withoutStutterStates (annotateStutteringSteps t') ^. observedStates . #elementStates)

prop_all_duplicates_are_stuttering :: Property
prop_all_duplicates_are_stuttering = forAll (Gen.observedState >>= Gen.traceWithState . pure) $ \t -> do
  lengthOf
    (observedStates . #elementStates)
    (withoutStutterStates (annotateStutteringSteps t))
    === 1