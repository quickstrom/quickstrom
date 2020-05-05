{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module WTP.LogicTest where

import Algebra.Lattice
import Test.QuickCheck ((===), forAll, listOf)
import Test.Tasty.Hspec
import qualified WTP.Formula.Logic as Logic
import WTP.Formula.Syntax hiding ((===))
import qualified WTP.Gen as Gen
import WTP.Result
import WTP.Verify
import Prelude hiding (Bool (..), not)

spec_nnf :: Spec
spec_nnf =
  describe "Logic" $ do
    let testFormula formula input result =
          let f = (Logic.simplify formula)
           in it (show input <> " ⊢ " <> show formula <> " (" <> show f <> ")") $
                verify f input `shouldBe` result
    describe "Always" $ do
      testFormula (always top) [mempty] Accepted
      testFormula (always top) [] Rejected

prop_logic_always = forAll ((,) <$> Gen.trueSyntax <*> Gen.trace) $ \(p, trace) -> do
  verify (Logic.simplify (always p)) trace === Accepted

prop_logic_any_false = forAll ((,) <$> Gen.falseSyntax <*> Gen.nonEmpty (listOf (pure mempty))) $ \(p, trace) -> do
    let p' = (Logic.simplify p)
    Rejected === verify p' trace