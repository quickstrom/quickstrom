{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.VerifyTest where

import qualified Data.Aeson as JSON
import qualified Data.Bool as Bool
import qualified Data.HashMap.Strict as HashMap
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
import Algebra.Lattice (fromBool)


assertMem :: Eq a => a -> [a] -> Result
assertMem = (\c s -> fromBool (elem c s))

verifyDOM :: Formula -> [Step] -> Result
verifyDOM f = NNF.verifyWith assertQuery (toNNF f)

verifyNNF :: Eq a => NNF.FormulaWith (NNF.Negation a) -> [[a]] -> Result
verifyNNF f = NNF.verifyWith assertMem f

spec_verify :: Spec
spec_verify = do
  describe "verify" $ do
    it "verifies with true for one step" $ do
      verifyDOM True [Step mempty mempty] `shouldBe` Accepted
    it "verifies with get and assertion" $ do
      let classList = JSON.toJSON ["foo", "bar" :: Text]
      verifyDOM
        ( (traverse (get (Property "classList")) =<< query "#some-element")
            ≡ Just classList
        )
        [ Step
            (HashMap.singleton "#some-element" [Element "a"])
            ( HashMap.singleton
                (Element "a")
                [ElementStateValue (Property "classList") classList]
            )
        ]
        `shouldBe` Accepted
    it "verifies with get and satisfy" $ do
      verifyDOM
        (queryAll "p" ⊢ ((== 2) . length))
        [Step (HashMap.singleton "p" [Element "a", Element "b"]) mempty]
        `shouldBe` Accepted
    it "is True with (And True True)" $ do
      verifyDOM (And True True) [Step HashMap.empty mempty] `shouldBe` Accepted
    it "is True with (Not False)" $ do
      verifyDOM (Not False) [Step HashMap.empty mempty] `shouldBe` Accepted
    it "works with Until" $ do
      verifyDOM (True `Until` True) [Step mempty mempty, Step mempty mempty] `shouldBe` Accepted
    it "verifies button example" $ do
      let steps =
            [ Step mempty mempty,
              Step
                ( HashMap.fromList
                    [ (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                      (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
                    ]
                )
                ( HashMap.fromList
                    [ (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [ElementStateValue Enabled Bool.True]),
                      (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [ElementStateValue Text ""])
                    ]
                ),
              Step
                ( HashMap.fromList
                    [ (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                      (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
                    ]
                )
                ( HashMap.fromList
                    [ (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [ElementStateValue Enabled Bool.False]),
                      (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [ElementStateValue Text "Boom!"])
                    ]
                )
            ]
      let buttonIsEnabled enabled = do
            (traverse (get Enabled) =<< query "button") ≡ Just enabled
          messageIs message =
            (traverse (get Text) =<< query ".message") ≡ Just message
          property =
            Eventually
              ( buttonIsEnabled Bool.True
                  `Until` (messageIs "Boom!" ∧ buttonIsEnabled Bool.False)
              )
      verifyDOM property steps `shouldBe` Accepted
  describe "NNF and Minimal equivalence" $ do
    it "works for Always True" $ do
      let f = Always True
          s = ["a"]
      verifyNNF (toNNF f) s `shouldBe` Minimal.verify (simplify f) s

hprop_minimal_and_nnf_equivalent :: Property
hprop_minimal_and_nnf_equivalent = withTests 10000 . property $ do
  syntax <- forAll Gen.anySyntax
  s <- forAll (Gen.trace (Range.linear 1 10))
  -- NNF
  let nnf = toNNF syntax
  annotateShow nnf
  let r1 = verifyNNF nnf s
  annotateShow r1
  -- Minimal
  let minimal = simplify syntax
  annotateShow minimal
  let r2 = Minimal.verifyWith assertMem minimal s
  annotateShow r2
  r1 === r2
