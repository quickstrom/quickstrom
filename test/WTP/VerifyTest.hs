{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module WTP.VerifyTest where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict       as HashMap
import           Prelude                   hiding (Bool (..), not)
import           Test.Tasty.Hspec
import           WTP.Syntax
import           WTP.Verify
import Data.Text (Text)
import qualified Data.Bool as Bool

verify' :: Formula -> [Step] -> Result
verify' = verify . simplify

spec_verify :: Spec
spec_verify = describe "verify" $ do

  it "is undetermined with true for zero steps" $ do
    verify' True [] `shouldBe` Undetermined
  it "verifies with true for one step" $ do

    verify' True [Step mempty mempty] `shouldBe` Accepted
  it "verifies with get and assertion" $ do
    let classList = JSON.toJSON ["foo", "bar" :: Text]
    verify'
        ((traverse (get (Property "classList")) =<< query "#some-element")
        ≡ Just classList
        )
        [ Step
            (HashMap.singleton "#some-element" [Element "a"])
            (HashMap.singleton
              (Element "a")
              [ElementStateValue (Property "classList") classList]
            )
        ]
      `shouldBe` Accepted
  it "verifies with get and satisfy" $ do
    verify' (queryAll "p" ⊢ ((== 2) . length))
            [Step (HashMap.singleton "p" [Element "a", Element "b"]) mempty]

      `shouldBe` Accepted

  it "is True with (And True True)" $ do
    verify' (And True True) [Step HashMap.empty mempty] `shouldBe` Accepted

  it "is True with (Not False)" $ do
    verify' (Not False) [Step HashMap.empty mempty] `shouldBe` Accepted

  it "works with Until" $ do
    verify' (True `Until` True) [Step mempty mempty, Step mempty mempty] `shouldBe` Accepted

  it "verifies button example" $ do
    let steps = [
            Step mempty mempty,
            Step 
              (HashMap.fromList [
                (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
              ])
              (HashMap.fromList [
                (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [ElementStateValue Enabled Bool.True]),
                (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [ElementStateValue Text ""])
              ]),
            Step 
              (HashMap.fromList [
                (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
              ])
              (HashMap.fromList [
                (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [ElementStateValue Enabled Bool.False]),
                (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [ElementStateValue Text "Boom!"])
              ])
          ]

    let 
        buttonIsEnabled enabled = do
          (traverse (get Enabled) =<< query "button") ≡ Just enabled
        messageIs message =
          (traverse (get Text) =<< query ".message") ≡ Just message
        property = Eventually
          (
           buttonIsEnabled Bool.True
              `Until` (messageIs "Boom!" ∧ buttonIsEnabled Bool.False)
          )
    verify' property steps `shouldBe` Accepted




