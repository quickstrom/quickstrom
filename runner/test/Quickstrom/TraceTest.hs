{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module Quickstrom.TraceTest where

import Control.Lens
import qualified Quickstrom.Gen as Gen
import Data.Generics.Labels ()
import Quickstrom.Prelude
import Quickstrom.Trace
import Test.QuickCheck (Property, forAll, (===))

prop_all_empty_are_stuttering :: Property
prop_all_empty_are_stuttering = forAll Gen.trace $ \t -> do
  let t' = t & observedStates .~ mempty
  (t' ^. observedStates . #elementStates) === (withoutStutterStates (annotateStutteringSteps t') ^. observedStates . #elementStates)
