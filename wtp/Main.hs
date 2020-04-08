{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import qualified Data.Bool as Bool
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Tree as Tree
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
        let result = verify (property spec) steps
        _ <- liftWebDriverTT (liftIO (putStrLn (Tree.drawTree (show <$> result))))
        assertEqual (stepResult (Tree.rootLabel result)) Accepted "verification using WebDriver"
  void $
    execWebDriverT
      defaultWebDriverConfig
      (runIsolated defaultFirefoxCapabilities (test simplified))

-- Simple example: a button that can be clicked, which then shows a message
example :: FilePath -> Specification Formula
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

buttonIsEnabled :: Formula
buttonIsEnabled = (traverse (get Enabled) =<< query "button") ≡ Just Bool.True

messageIs :: Text -> Formula
messageIs message =
  (traverse (get Text) =<< query ".message") ≡ Just message
