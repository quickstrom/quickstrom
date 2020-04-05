{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.IO.Class (liftIO)
import Web.Api.WebDriver
import System.Directory
import qualified WTP.Run as WTP
import WTP.Core (Path (..))
import WTP.Syntax
import WTP.Property
import WTP.Verify

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let ex = example cwd
      simplified = WTP.Property.Property
        { actions = actions ex,
          specification = simplify (specification ex)
        }
  let test prop = do
        steps <- WTP.run prop
        liftWebDriverTT (liftIO (print steps))
        assertEqual (run (runError (verify (specification prop) steps))) (Right ()) "run failed"
  void $ execWebDriverT
    defaultWebDriverConfig
    (runIsolated defaultFirefoxCapabilities (test simplified))

--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

-- Simple example, a form for posting a comment. Note that you can only post once, even
-- if there's an error.
example :: FilePath -> Property Formula
example cwd =
  WTP.Property.Property
  { actions = [ Navigate (Path ("file://" <> Text.pack cwd <> "/test/button.html"))
              , Click "button"
              ]
  , specification =
      buttonIsDisabled Prelude.False
        `Until`
      (messageIs "Boom!" ∧ buttonIsDisabled Prelude.True)
  }
  where
    buttonIsDisabled disabled = do
      let q = do
            v <- get "disabled" =<< require =<< query "button"
            case v of
              JSON.Bool b -> pure b
              _ -> pure Prelude.False
      q ≡ disabled
    messageIs message =
        (get InnerText =<< require =<< query ".message") ≡ message


  {-
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
-}
