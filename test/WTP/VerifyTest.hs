{-# LANGUAGE OverloadedStrings #-}
module WTP.VerifyTest where

import Prelude hiding (Bool (..), not)
import Test.Tasty.Hspec
import qualified Data.HashMap.Strict as HashMap
import WTP.Verify
import WTP.Syntax


spec_verify :: Spec
spec_verify = describe "verify" $ do
  it "is undetermined with true for zero steps" $ do
    verify (simplify True) []
      `shouldBe` Left Undetermined
  it "verifies with true for one step" $ do
    verify (simplify True) [Step HashMap.empty]
      `shouldBe` Right ()
  it "verifies with get and assertion" $ do
    verify
      ( simplify
          ( Get
              ClassList
              (Require (Query "#some-element"))
              ≡ ["foo", "bar"]
          )
      )
      [Step (HashMap.singleton "#some-element" [Element])]
      `shouldBe` Right ()


--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

-- Simple example, a form for posting a comment. Note that you can only post once, even
-- if there's an error.
example :: Formula
example =
        ( hasMessage
            "Post a comment below."
            ["message", "info"]
            ∧ spinnerIs Hidden
        )
          `Until` spinnerIs Active
          `Until` ( Always
                      ( hasMessage
                          "Failed to post comment."
                          ["message", "error"]
                          ∧ spinnerIs Hidden
                      )
                      ∨ Always
                        ( hasMessage
                            "Form posted!"
                            ["message", "error"]
                            ∧ spinnerIs Hidden
                        )
                  )
  where
    spinnerIs state =
      Get
        ClassList
        (Require (Query ".my-app .spinner"))
        ≡ case state of
          Active -> ["spinner", "active"]
          Hidden -> ["spinner"]
    hasMessage message classes =
      ( Get
          ClassList
          (Require (Query ".my-app .message"))
          ≡ classes
      )
        ∧ ( Get InnerText (Require (Query ".my-app .message"))
              ≡ message
          )
