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

verifyNNF :: Eq a => NNF.FormulaWith (NNF.Negation a) -> [a] -> Result
verifyNNF f = NNF.stepResult . Tree.rootLabel . NNF.verify f

spec_nnf :: Spec
spec_nnf =
  describe "NNF" $ do
    it "Always True" $ do
      let f = Always True
          s = "a"
      verifyNNF (toNNF f) s `shouldBe` Accepted
    it "Release True False" $ do
      let f = Release (Assert 'a') (Assert 'b')
          s = "a"
      verifyNNF (toNNF f) s `shouldBe` Accepted
    it "Release False True" $ do
      let f = Release False True
          s = "a"
      verifyNNF (toNNF f) s `shouldBe` Accepted
    it "Release True True" $ do
      let f = Release True True
          s = "a"
      verifyNNF (toNNF f) s `shouldBe` Accepted
    it "Release False False" $ do
      let f = Release False False
          s = "a"
      verifyNNF (toNNF f) s `shouldBe` Rejected