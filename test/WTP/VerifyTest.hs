{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.VerifyTest where

import Algebra.Lattice (bottom, top)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Test.Tasty.Hspec hiding (Selector)
import WTP.Element
import WTP.Result
import WTP.Syntax
import qualified WTP.Trace as Trace
import WTP.Verify
import Prelude hiding (Bool (..), all, length, map, seq)

verify' :: Formula -> [Trace.ObservedState] -> Result
verify' = flip verify

spec_verify :: Spec
spec_verify = do
  it "verifies with get and assertion" $ do
    let classList = JSON.toJSON ["foo", "bar" :: Text]
    let q = property "classList" (byCss "#some-element")
    verify'
      (queryOne q === lit classList)
      [ Trace.ObservedState
          (HashMap.singleton q [classList])
      ]
      `shouldBe` Accepted
  it "verifies with get and satisfy" $ do
    let q = byCss "p"
    verify'
      (length (queryAll q) === num 2)
      [ Trace.ObservedState
          ( HashMap.singleton
              q
              [ JSON.toJSON (Element "a"),
                JSON.toJSON (Element "b")
              ]
          )
      ]
      `shouldBe` Accepted
  it "is top with (top /\\ top)" $ do
    verify' (top /\ top) [Trace.ObservedState mempty] `shouldBe` Accepted
  it "is true with (next false \\/ true)" $ do
    verify' (next bottom \/ top) [Trace.ObservedState mempty] `shouldBe` Accepted
  it "is false with (next top) when empty trace" $ do
    verify' (next top) [] `shouldBe` Rejected
  it "is top with (neg bottom)" $ do
    verify' (neg bottom) [Trace.ObservedState mempty] `shouldBe` Accepted
  it "verifies button example" $ do
    let steps =
          [ Trace.ObservedState
              ( HashMap.fromList
                  [ (enabled (byCss "button"), [JSON.Bool top]),
                    (text (byCss ".message"), [""])
                  ]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [ (enabled (byCss "button"), [JSON.Bool bottom]),
                    (text (byCss ".message"), ["Boom!"])
                  ]
              )
          ]
    let buttonIsEnabled = queryOne (enabled (byCss "button")) === top
        messageIs message = queryOne (text (byCss ".message")) === message
        prop =
          -- TODO: define actions using primed queries
          always (buttonIsEnabled \/ (messageIs "Boom!" /\ neg buttonIsEnabled))
    verify' prop steps `shouldBe` Accepted
  it "verifies changing input state" $ do
    let steps =
          [ Trace.ObservedState
              ( HashMap.fromList
                  [(property "value" (byCss "input"), [JSON.String "a"])]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [(property "value" (byCss "input"), [JSON.String ""])]
              )
          ]
    let inputIs :: Formula -> Formula
        inputIs t = queryOne (property "value" (byCss "input")) === t
        prop = inputIs "a" /\ next (inputIs "")
    verify' prop steps `shouldBe` Accepted
