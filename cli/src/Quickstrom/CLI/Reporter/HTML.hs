{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Quickstrom.CLI.Reporter.HTML where

import Control.Lens
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Text.Prettyprint.Doc (annotate, line, pretty, (<+>))
import Prettyprinter.Render.Terminal (Color (..), color)
import qualified Quickstrom.CLI.Logging as Quickstrom
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude
import qualified Quickstrom.Pretty as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Trace as Quickstrom

htmlReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => Quickstrom.Reporter m
htmlReporter _webDriverOpts checkOpts = \case
  Quickstrom.CheckFailure {Quickstrom.failedAfter, Quickstrom.failingTest} -> do
    let withoutStutters = Quickstrom.withoutStutterStates (Quickstrom.trace failingTest)
    let screenshots = withoutStutters ^.. Quickstrom.observedStates . #screenshot . _Just
    Quickstrom.logDoc (Quickstrom.logSingle Nothing (Quickstrom.prettyTrace withoutStutters))
    void . ifor screenshots $ \i screenshot -> do
      liftIO (BS.writeFile ("/tmp/quickstrom-screenshot-" <> show i <> "-" <> show (hash screenshot) <> ".png") screenshot)
    case Quickstrom.reason failingTest of
      Just err -> Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
      Nothing -> pure ()
    Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
      line <> "Failed after" <+> pretty failedAfter <+> "tests and" <+> pretty (Quickstrom.numShrinks failingTest) <+> "levels of shrinking." <> line
  Quickstrom.CheckError {Quickstrom.checkError} -> do
    Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
      line <> "Check encountered an error:" <+> pretty checkError <> line
  Quickstrom.CheckSuccess ->
    Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Green) $
      line <> "Passed" <+> pretty (Quickstrom.checkTests checkOpts) <+> "tests." <> line
