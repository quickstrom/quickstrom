{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import qualified Data.Bool as Bool
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Lens
import qualified Data.Tree as Tree
import System.Directory
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Formula.Syntax
import WTP.Verify
import Web.Api.WebDriver
import qualified WTP.Formula.Minimal as Minimal
import WTP.Result (Result(Accepted))

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let ex = example cwd
      simplified :: Specification Minimal.Formula = ex & _ %~ simplify
  let test spec = do
        steps <- WTP.run spec
        let result = Minimal.verifyWith assertQuery (property spec) steps
        -- _ <- liftWebDriverTT (liftIO (putStrLn (Tree.drawTree (drawVerificationTree result))))
        assertEqual result Accepted "verification using WebDriver"
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
