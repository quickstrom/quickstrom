{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bool as Bool
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory
import WTP.Formula (IsQuery)
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
        -- _ <- liftWebDriverTT (liftIO (mapM print steps))
        result <- runM (verify (property spec) steps)
        assertEqual result Accepted "run failed"
  void $
    execWebDriverT
      defaultWebDriverConfig
      (runIsolated defaultFirefoxCapabilities (test simplified))


-- Simple example: a button that can be clicked, which then shows a message
example :: FilePath -> Specification Formula effs
example cwd =
  Specification
    { actions =
        [ Navigate (Path ("file://" <> Text.pack cwd <> "/test/button.html")),
          Click "button"
        ],
      property =
        Eventually $
          buttonIsEnabled
            `Until` (messageIs "Boom!" ∧ Not buttonIsEnabled)
    }

buttonIsEnabled :: IsQuery effs => Formula effs
buttonIsEnabled = (traverse (get Enabled) =<< query "button") ≡ Just Bool.True

messageIs :: IsQuery effs => Text -> Formula effs
messageIs message =
  (traverse (get Text) =<< query ".message") ≡ Just message
