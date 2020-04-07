{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Web.Api.WebDriver
import System.Directory
import qualified WTP.Run as WTP
import WTP.Syntax
import WTP.Specification
import WTP.Verify

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let ex = example cwd
      simplified = Specification
        { actions = actions ex,
          property = simplify (property ex)
        }
  let test spec = do
        steps <- WTP.run spec
        result <- runM (runError (verify (property spec) steps))
        assertEqual result (Right ()) "run failed"
  void $ execWebDriverT
    defaultWebDriverConfig
    (runIsolated defaultFirefoxCapabilities (test simplified))

--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

-- Simple example, a form for posting a comment. Note that you can only post once, even
-- if there's an error.
example :: FilePath -> Specification Formula effs
example cwd =
  Specification
  { actions = [ Navigate (Path ("file://" <> Text.pack cwd <> "/test/button.html"))
              , Click "button"
              ]
  , property =
      buttonIsEnabled Prelude.True
        `Until`
      (messageIs "Boom!" ∧ buttonIsEnabled Prelude.False)
  }
  where
    buttonIsEnabled enabled = do
      (get Enabled =<< require =<< query "button") ≡ enabled
    messageIs message =
        (get (Property "innerText") =<< require =<< query ".message") ≡ message


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
