{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.NNFTest where

import Algebra.Lattice
import qualified Data.List as List
import Test.QuickCheck ((===), forAll, listOf, withMaxSuccess)
import Test.Tasty.Hspec
import qualified WTP.Formula.NNF as NNF
import WTP.Formula.Syntax hiding ((===))
import qualified WTP.Gen as Gen
import WTP.Result
import Prelude hiding (Bool (..), not)

verifyNNF :: NNF.FormulaWith (NNF.Negation Char) -> [String] -> Result
verifyNNF f = NNF.verifyWith (\c s -> fromBool (c `elem` s)) f

spec_nnf :: Spec
spec_nnf =
  describe "NNF" $ do
    let testFormula formula input result =
          let nnf = (toNNF formula)
           in it (show input <> " ⊢ " <> show formula <> " (" <> show nnf <> ")") $
                verifyNNF nnf input `shouldBe` result
    describe "Next" $ do
      let isLast = Not (Next (And True True))
      testFormula isLast ["a"] Accepted
    describe "Always" $ do
      testFormula (Always True) ["a"] Accepted
      testFormula (Always True) [] Rejected
    describe "Until" $ do
      testFormula (Until (Assert 'a') (Assert 'b')) ["a", "b", "c"] Accepted
      testFormula (Until (Assert 'a') (Assert 'b')) ["a", "b"] Accepted
    describe "Release" $ do
      testFormula (Release False True) [] Rejected
      testFormula (Release True True) [] Rejected
      testFormula (Release True False) ["a"] Rejected
      testFormula (Release True True) ["a"] Accepted
      testFormula (Release False True) ["a"] Accepted
      testFormula (Release False False) ["a"] Rejected
      testFormula (Release (Assert 'b') (Assert 'a')) ["a", "ab", "b"] Accepted

prop_nnf_always = forAll ((,) <$> Gen.anySyntax <*> Gen.trace) $ \(p, trace) -> do
  verifyNNF (toNNF (Always p)) trace === verifyNNF (toNNF (False `Release` p)) trace

prop_nnf_any_true = forAll ((,) <$> Gen.trueSyntax (pure 'a') <*> (map (List.nub . ("a" <>)) <$> Gen.nonEmpty Gen.trace)) $ \(p, trace) -> do
  let nnf = (toNNF p)
  Accepted === verifyNNF nnf trace

prop_nnf_any_false = forAll ((,) <$> Gen.falseSyntax Gen.variable <*> Gen.nonEmpty (listOf (pure []))) $ \(p, trace) -> do
    let nnf = (toNNF p)
    Rejected === verifyNNF nnf trace
