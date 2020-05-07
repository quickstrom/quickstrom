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
import WTP.Result
import WTP.Syntax
import qualified WTP.Trace as Trace
import WTP.Verify
import Prelude hiding (Bool (..), all, map, seq)

verify' :: Proposition -> [Trace.ObservedState] -> Result
verify' = flip verify

spec_verify :: Spec
spec_verify = do
  it "verifies with get and assertion" $ do
    let classList = JSON.toJSON ["foo", "bar" :: Text]
    verify'
      ((query (fromMaybe (JSON.Array mempty) <$> (traverse (property "classList") =<< one "#some-element"))) === json classList)
      [ Trace.ObservedState
          (HashMap.singleton "#some-element" [Element "a"])
          ( HashMap.singleton
              (Element "a")
              [Trace.ElementStateValue (Property "classList") classList]
          )
      ]
      `shouldBe` Accepted
  it "verifies with get and satisfy" $ do
    verify'
      ((length <$> query (all "p")) === num 2)
      [Trace.ObservedState (HashMap.singleton "p" [Element "a", Element "b"]) mempty]
      `shouldBe` Accepted
  it "is top with (top /\\ top)" $ do
    verify' (top /\ top) [Trace.ObservedState HashMap.empty mempty] `shouldBe` Accepted
  it "is true with (next false \\/ true)" $ do
    verify' (next bottom \/ top) [Trace.ObservedState HashMap.empty mempty] `shouldBe` Accepted
  it "is false with (next top) when empty trace" $ do
    verify' (next top) [] `shouldBe` Rejected
  it "is top with (neg bottom)" $ do
    verify' (neg bottom) [Trace.ObservedState HashMap.empty mempty] `shouldBe` Accepted
  it "verifies button example" $ do
    let steps =
          [ Trace.ObservedState
              ( HashMap.fromList
                  [ (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                    (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
                  ]
              )
              ( HashMap.fromList
                  [ (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [Trace.ElementStateValue Enabled top]),
                    (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [Trace.ElementStateValue Text ""])
                  ]
              ),
            Trace.ObservedState
              ( HashMap.fromList
                  [ (Selector ".message", [Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6"]),
                    (Selector "button", [Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd"])
                  ]
              )
              ( HashMap.fromList
                  [ (Element "7485ccda-3534-4f9d-9e0e-7bfccdf70abd", [Trace.ElementStateValue Enabled bottom]),
                    (Element "f93eb8a0-3511-4a79-8cdf-eef9a8beb5b6", [Trace.ElementStateValue Text "Boom!"])
                  ]
              )
          ]
    let buttonIsEnabled = fromMaybe bottom <$> query (traverse enabled =<< one "button")
        messageIs message = query (traverse text =<< one ".message") === (Just <$> message)
        prop =
          -- TODO: define actions using primed queries
          always (buttonIsEnabled \/ (messageIs "Boom!" /\ neg buttonIsEnabled))
    verify' prop steps `shouldBe` Accepted
