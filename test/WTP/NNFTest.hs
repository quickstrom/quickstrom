{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.NNFTest where

import Algebra.Lattice
import qualified Data.Bool as Bool
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Tree as Tree
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec
import qualified WTP.Formula.Minimal as Minimal
import qualified WTP.Formula.NNF as NNF
import WTP.Formula.Syntax hiding ((===))
import qualified WTP.Gen as Gen
import WTP.Result
import WTP.Verify
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

hprop_nnf_always = property $ do
  p <- forAll Gen.anySyntax
  trace <- forAll (Gen.trace (Range.linear 0 10))
  verifyNNF (toNNF (Always p)) trace === verifyNNF (toNNF (False `Release` p)) trace

hprop_nnf_any_true = property $ do
  p <- forAll (Gen.trueSyntax (pure 'a'))
  trace <- forAll (map (List.nub . ("a" <>)) <$> Gen.trace (Range.linear 2 10))
  let nnf = (toNNF p)
  annotateShow nnf
  Accepted === verifyNNF nnf trace

hprop_nnf_any_false = property $ do
  p <- forAll (Gen.falseSyntax Gen.variable)
  trace <- forAll (Gen.list (Range.linear 2 10) (pure []))
  let nnf = (toNNF p)
  annotateShow nnf
  Rejected === verifyNNF nnf trace
