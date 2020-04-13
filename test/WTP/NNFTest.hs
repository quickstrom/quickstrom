{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.NNFTest where

import qualified Data.Aeson as JSON
import qualified Data.Bool as Bool
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Tree as Tree
import Test.Tasty.Hspec
import WTP.Formula.Syntax hiding ((===))
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Minimal as Minimal
import WTP.Verify
import WTP.Result
import Prelude hiding (Bool (..), not)
import Hedgehog 
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Algebra.Lattice

verifyNNF :: NNF.FormulaWith (NNF.Negation Char) -> [String] -> Result
verifyNNF f = NNF.stepResult . Tree.rootLabel . NNF.verifyWith (\c s -> fromBool (c `elem` s)) f

spec_nnf :: Spec
spec_nnf =
  describe "NNF" $ do
    let testFormula formula input result =
          it (show input <> " ⊢ " <> show formula) $ do
            verifyNNF (toNNF formula) input `shouldBe` result

    describe "Always" $ do
      testFormula (Always True) ["a"] Accepted
      testFormula (Always True) [] Accepted

    describe "Until" $ do
      testFormula (Until (Assert 'a') (Assert 'b')) ["a", "b", "c"] Accepted
      testFormula (Until (Assert 'a') (Assert 'b')) [] Accepted

    describe "Release" $ do
      testFormula (Release False True) [] Accepted
      testFormula (Release True True) [] Accepted
      testFormula (Release True False) ["a"] Rejected
      testFormula (Release True True) ["a"] Accepted
      testFormula (Release False True) ["a"] Accepted
      testFormula (Release False False) ["a"] Rejected
      testFormula (Release (Assert 'b') (Assert 'a')) ["a", "ab", "b"] Accepted