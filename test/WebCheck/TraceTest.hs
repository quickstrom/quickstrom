{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.TraceTest where

import Control.Lens
import Test.QuickCheck ((===), forAll, Property)
import qualified WebCheck.Gen as Gen
import WebCheck.Trace

prop_all_empty_are_stuttering :: Property
prop_all_empty_are_stuttering = forAll Gen.trace $ \t -> do
  let t' = t & observedStates  .~ mempty
  (t' ^. observedStates) === (withoutStutterStates (annotateStutteringSteps t') ^. observedStates)
