{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.VerifyTest where

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

verifyDOM :: Formula -> [Step] -> Result
verifyDOM f = NNF.stepResult . Tree.rootLabel . NNF.verifyWith assertQuery (toNNF f)

verifyNNF :: Eq a => NNF.FormulaWith (NNF.Negation a) -> [a] -> Result
verifyNNF f = NNF.stepResult . Tree.rootLabel . NNF.verify f

spec_verify :: Spec
spec_verify = describe "verify" $ do
  it "is accepted with true for zero steps" $ do
    verifyDOM True [] `shouldBe` Accepted
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

hprop_minimal_and_nnf_equivalent :: Property
hprop_minimal_and_nnf_equivalent = property $ do
  syntax <- forAll genSyntax
  s <- forAll genString

  -- NNF
  let nnf = toNNF syntax
  annotateShow nnf
  let r1 = verifyNNF nnf s
  annotateShow r1

  -- Minimal
  let minimal = simplify syntax
  annotateShow minimal
  let r2 = Minimal.verify minimal s
  annotateShow r2

  r1 === r2

genChar :: Gen Char
genChar = Gen.alpha

genString :: Gen String
genString = Gen.string (Range.linear 1 10) genChar

genSyntax :: Gen (FormulaWith Char)
genSyntax =
  Gen.small $
    Gen.recursive
      Gen.choice
      [ pure True,
        pure False,
        Assert <$> genChar
      ]
      [ Gen.subterm genSyntax Not,
        Gen.subterm2 genSyntax genSyntax And,
        Gen.subterm2 genSyntax genSyntax Or,
        Gen.subterm2 genSyntax genSyntax Until,
        Gen.subterm2 genSyntax genSyntax Implies,
        Gen.subterm2 genSyntax genSyntax Equivalent,
        Gen.subterm genSyntax Always,
        Gen.subterm genSyntax Eventually
      ]