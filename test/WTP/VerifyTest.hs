{-# LANGUAGE OverloadedStrings #-}
module WTP.VerifyTest where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Data.HashMap.Strict       as HashMap
import           Prelude                   hiding (Bool (..), not)
import           Test.Tasty.Hspec
import           WTP.Syntax
import           WTP.Verify

verify' :: Formula -> [Step] -> Either Failure ()
verify' formula steps = run (runError (verify (simplify formula) steps))

spec_verify :: Spec
spec_verify = describe "verify" $ do

  it "is undetermined with true for zero steps" $ do
    verify' True []
      `shouldBe` Left Undetermined
  it "verifies with true for one step" $ do

    verify' True [Step HashMap.empty]
      `shouldBe` Right ()
  it "verifies with get and assertion" $ do
    verify'
        ((get ClassList =<< require =<< query "#some-element") ≡ ["foo", "bar"])
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
      (get ClassList =<< require =<< query ".my-app .spinner")
        ≡ case state of
          Active -> ["spinner", "active"]
          Hidden -> ["spinner"]
    hasMessage message classes =
      ((get ClassList =<< require =<< query ".my-app .message") ≡ classes
      )
        ∧ ( (get InnerText =<< require =<< query ".my-app .message")
              ≡ message
          )
