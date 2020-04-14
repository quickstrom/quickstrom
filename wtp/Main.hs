{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bool as Bool
import Data.Function ((&))
import Data.Generics.Product
import Data.Generics.Sum
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Tree as Tree
import qualified Hedgehog as Hedgehog
import qualified Hedgehog.Main as Hedgehog
import System.Directory
import qualified WTP.Formula.NNF as NNF
import WTP.Formula.Syntax
import WTP.Result (Result (Rejected, Accepted))
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Verify
import Web.Api.WebDriver hiding (runIsolated)
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  Hedgehog.defaultMain [Hedgehog.check (prop cwd)]

prop :: FilePath -> Hedgehog.Property
prop cwd = (Hedgehog.withTests 2 (WTP.asProperty (example cwd)))

-- Simple example: a button that can be clicked, which then shows a message
example :: FilePath -> Specification Formula
example cwd =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      actions =
        [Click "button"],
      property =
        buttonIsEnabled
          `Until` (messageIs "Boom!" ∧ Not buttonIsEnabled)
    }

buttonIsEnabled :: Formula
buttonIsEnabled = (traverse (get Enabled) =<< query "button") ≡ Just Bool.True

messageIs :: Text -> Formula
messageIs message =
  (traverse (get Text) =<< query ".message") ≡ Just message
