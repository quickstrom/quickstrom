{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.VerifyTest where

import Algebra.Lattice (bottom, top)
import qualified Data.HashMap.Strict as HashMap
import Test.Tasty.Hspec hiding (Selector)
import WTP.Element
import WTP.Result
import WTP.Syntax
import qualified WTP.Trace as Trace
import WTP.Value
import WTP.Verify
import Prelude hiding (Bool (..), all, length, map, seq)

verify' :: Formula -> [Trace.ObservedState] -> Either EvalError Result
verify' = flip verify

spec_verify :: Spec
spec_verify = do
  it "verifies with get and assertion" $ do
    let q = property "classList" (byCss "#some-element")
    verify'
      (queryOne q === ["foo", "bar"])
      [ Trace.ObservedState
          (HashMap.singleton q [VSeq ["foo", "bar"]])
      ]
      `shouldBe` pure Accepted
  it "verifies with get and satisfy" $ do
    let q = byCss "p"
    verify'
      (apply length [queryAll q] === num 2)
      [ Trace.ObservedState
          ( HashMap.singleton
              q
              [ VElement (Element "a"),
                VElement (Element "b")
              ]
          )
      ]
      `shouldBe` pure Accepted
  it "is top with (top /\\ top)" $ do
    verify' (top /\ top) [Trace.ObservedState mempty] `shouldBe` pure Accepted
  it "is true with (next false \\/ true)" $ do
    verify' (next bottom \/ top) [Trace.ObservedState mempty] `shouldBe` pure Accepted
  it "is true with (true \\/ next false)" $ do
    verify' (top \/ next bottom) [Trace.ObservedState mempty] `shouldBe` pure Accepted
  it "is false with (next top) when empty trace" $ do
    verify' (next top) [] `shouldBe` Left Undetermined
  it "is top with (neg bottom)" $ do
    verify' (neg bottom) [Trace.ObservedState mempty] `shouldBe` pure Accepted
  it "verifies button example" $ do
    let steps =
          [ Trace.ObservedState
              ( HashMap.fromList
                  [ (enabled (byCss "button"), [VBool top]),
                    (text (byCss ".message"), [""])
                  ]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [ (enabled (byCss "button"), [VBool bottom]),
                    (text (byCss ".message"), ["Boom!"])
                  ]
              )
          ]
    let buttonIsEnabled = queryOne (enabled (byCss "button")) === top
        messageIs message = queryOne (text (byCss ".message")) === message
        prop =
          -- TODO: define actions using primed queries
          always (buttonIsEnabled \/ (messageIs "Boom!" /\ neg buttonIsEnabled))
    verify' prop steps `shouldBe` pure Accepted
  it "verifies changing input state" $ do
    let steps =
          [ Trace.ObservedState
              ( HashMap.fromList
                  [(property "value" (byCss "input"), [VString "a"])]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [(property "value" (byCss "input"), [VString ""])]
              )
          ]
    let inputIs :: Formula -> Formula
        inputIs t = queryOne (property "value" (byCss "input")) === t
        prop = inputIs "a" /\ next (inputIs "")
    verify' prop steps `shouldBe` pure Accepted
