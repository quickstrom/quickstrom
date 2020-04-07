{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.Freer
import qualified Data.Text as Text
import System.Directory
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Syntax
import WTP.Verify
import Web.Api.WebDriver

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let ex = example cwd
      simplified =
        Specification
          { actions = actions ex,
            property = simplify (property ex)
          }
  let test spec = do
        steps <- WTP.run spec
        result <- runM (verify (property spec) steps)
        assertEqual result Accepted "run failed"
  void $
    execWebDriverT
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
    { actions =
        [ Navigate (Path ("file://" <> Text.pack cwd <> "/test/button.html")),
          Click "button"
        ],
      property =
        Eventually
          ( buttonIsEnabled Prelude.True
              `Until` (messageIs "Boom!" ∧ buttonIsEnabled Prelude.False)
          )
    }
  where
    buttonIsEnabled enabled = do
      (maybe (pure Prelude.False) (get Enabled) =<< query "button") ≡ enabled
    messageIs message =
      (maybe (pure Nothing) (fmap Just <$> get Text) =<< query ".message") ≡ Just message
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
