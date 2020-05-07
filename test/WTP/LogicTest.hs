{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module WTP.LogicTest where

import Algebra.Lattice
import Test.QuickCheck ((===), forAll, listOf)
import Test.Tasty.Hspec
import WTP.Formula
import WTP.Syntax hiding ((===))
import qualified WTP.Gen as Gen
import WTP.Result
import WTP.Verify
import Prelude hiding (Bool (..), not)

spec_logic :: Spec
spec_logic =
  describe "Logic" $ do
    let testFormula formula input result =
          let f = (simplify formula)
           in it (show input <> " ⊢ " <> show formula <> " (" <> show f <> ")") $
                verify input f `shouldBe` result
    describe "Always" $ do
      testFormula (always top) [mempty] Accepted
      testFormula (always top) [] Accepted

prop_logic_always = forAll ((,) <$> Gen.trueSyntax <*> Gen.trace) $ \(p, trace) -> do
  verify trace (simplify (always p)) === Accepted

prop_logic_any_false = forAll ((,) <$> Gen.falseSyntax <*> Gen.nonEmpty (listOf (pure mempty))) $ \(p, trace) -> do
  let p' = simplify p
  Rejected === verify trace p'
