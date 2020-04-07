{-# LANGUAGE OverloadedStrings #-}
module WTP.VerifyTest where

import qualified Data.Aeson as JSON
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Data.HashMap.Strict       as HashMap
import           Prelude                   hiding (Bool (..), not)
import           Test.Tasty.Hspec
import           WTP.Syntax
import           WTP.Verify
import Data.Text (Text)

verify' :: Formula -> [Step] -> Either Failure ()
verify' formula steps = run (runError (verify (simplify formula) steps))

spec_verify :: Spec
spec_verify = describe "verify" $ do

  it "is undetermined with true for zero steps" $ do
    verify' True [] `shouldBe` Left Undetermined
  it "verifies with true for one step" $ do

    verify' True [Step mempty mempty] `shouldBe` Right ()
  it "verifies with get and assertion" $ do
    let classList = JSON.toJSON ["foo", "bar" :: Text]
    verify'
        ( (get (Property "classList") =<< require =<< query "#some-element")
        ≡ classList
        )
        [ Step
            (HashMap.singleton "#some-element" [Element "a"])
            (HashMap.singleton
              (Element "a")
              [ElementStateValue (Property "classList") classList]
            )
        ]
      `shouldBe` Right ()
  it "verifies with get and satisfy" $ do
    verify' (queryAll "p" ⊢ ((== 2) . length))
            [Step (HashMap.singleton "p" [Element "a", Element "b"]) mempty]

      `shouldBe` Right ()

  it "is True with (And True True)" $ do
    verify' (And True True) [Step HashMap.empty mempty] `shouldBe` Right ()

  it "is True with (Not False)" $ do
    verify' (Not False) [Step HashMap.empty mempty] `shouldBe` Right ()


