{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.VerifyTest where

import Algebra.Lattice (bottom, top)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty.Hspec
import WTP.Element
import WTP.Query
import WTP.Result
import WTP.Syntax
import qualified WTP.Trace as Trace
import WTP.Verify
import Prelude hiding (Bool (..), all, map, seq)
import qualified Data.Bool as Bool

verify' :: Proposition -> [Trace.ObservedState] -> Result
verify' = flip verify

spec_verify :: Spec
spec_verify = do
  it "verifies with get and assertion" $ do
    let classList = JSON.toJSON ["foo", "bar" :: Text]
    let q = property "classList" (byCss "#some-element")
    verify'
      ((fromMaybe (JSON.Array mempty) . fmap propertyValue <$> (queryOne q)) === json classList)
      [ Trace.ObservedState
          ( HashMap.singleton
              (SomeQuery q)
              [Trace.SomeValue (PropertyValue classList)]
          )
      ]
      `shouldBe` Accepted
  it "verifies with get and satisfy" $ do
    let q = byCss "p"
    verify'
      ((length <$> queryAll q) === num 2)
      [ Trace.ObservedState
          ( HashMap.singleton
              (SomeQuery q)
              [ Trace.SomeValue (Element "a"),
                Trace.SomeValue (Element "b")
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
                  [ (SomeQuery (enabled (byCss "button")), [Trace.SomeValue Bool.True]),
                    (SomeQuery (text (byCss ".message")), [Trace.SomeValue (mempty :: Text)])
                  ]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [ (SomeQuery (enabled (byCss "button")), [Trace.SomeValue Bool.False]),
                    (SomeQuery (text (byCss ".message")), [Trace.SomeValue ("Boom!" :: Text)])
                  ]
              )
          ]
    let buttonIsEnabled = fromMaybe bottom <$> queryOne (enabled (byCss "button"))
        messageIs message = queryOne (text (byCss ".message")) === (Just <$> message)
        prop =
          -- TODO: define actions using primed queries
          always (buttonIsEnabled \/ (messageIs "Boom!" /\ neg buttonIsEnabled))
    verify' prop steps `shouldBe` Accepted
