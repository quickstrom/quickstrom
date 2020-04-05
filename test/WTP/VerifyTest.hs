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
  it "verifies with get and satisfy" $ do
    verify'
      (queryAll "p" ⊢ ((== 2) . length))
      [Step (HashMap.singleton "p" [Element, Element])]

      `shouldBe` Right ()

  it "is True with (And True True)" $ do
    verify' (And True True) [Step HashMap.empty]
      `shouldBe` Right ()

  it "is True with (Not False)" $ do
    verify' (Not False) [Step HashMap.empty]
      `shouldBe` Right ()


