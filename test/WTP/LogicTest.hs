{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module WebCheck.LogicTest where

import Algebra.Lattice
import Test.QuickCheck ((===), forAll, listOf)
import Test.Tasty.Hspec
import WebCheck.Formula
import WebCheck.Syntax hiding ((===))
import qualified WebCheck.Gen as Gen
import WebCheck.Result
import WebCheck.Verify
import Prelude hiding (Bool (..), not)
import qualified Data.List.NonEmpty as NonEmpty

spec_logic :: Spec
spec_logic =
  describe "Logic" $ do
    let testFormula formula input result =
          let f = (simplify formula)
           in it (show input <> " ⊢ " <> show formula <> " (" <> show f <> ")") $
                verify input f `shouldBe` result
    describe "Always" $ do
      testFormula (always top) [mempty] (pure Accepted)
      testFormula (always top) [] (pure Accepted)

prop_logic_always = forAll ((,) <$> Gen.trueSyntax <*> listOf Gen.observedState) $ \(p, trace) -> do
  verify trace (simplify (always p)) === pure Accepted

prop_logic_any_false = forAll ((,) <$> Gen.falseSyntax <*> Gen.nonEmpty (listOf (pure mempty))) $ \(p, trace) -> do
  let p' = simplify p
  pure Rejected === verify (NonEmpty.toList trace) p'
