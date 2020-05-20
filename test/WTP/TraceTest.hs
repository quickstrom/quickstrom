{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | 

module WTP.TraceTest where

import Control.Lens
import Test.QuickCheck ((===), forAll, listOf)
import Data.Generics.Product (position, field)
import qualified WTP.Gen as Gen
import WTP.Trace

prop_all_empty_are_stuttering = forAll Gen.trace $ \t -> do
  let t' = t & observedStates  .~ mempty
  (t' ^. observedStates) === (withoutStutterStates (annotateStutteringSteps t') ^. observedStates)
