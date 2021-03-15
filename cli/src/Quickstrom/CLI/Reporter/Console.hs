{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.CLI.Reporter.Console where

import Control.Lens
import Data.Generics.Labels ()
import Data.Text.Prettyprint.Doc (annotate, line, pretty, (<+>))
import Prettyprinter.Render.Terminal (Color (..), color)
import qualified Quickstrom.CLI.Logging as Quickstrom
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude
import qualified Quickstrom.Pretty as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import Prettyprinter (Doc)

consoleReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => Quickstrom.Reporter m
consoleReporter =
  Quickstrom.Reporter
    { Quickstrom.preCheck = \_ _ -> pure Quickstrom.OK,
      Quickstrom.report = \_ _ result ->
        case result of
          Quickstrom.CheckFailure {Quickstrom.failedAfter, Quickstrom.failedTest} -> do
            let trace' = failedTest ^. #trace
            Quickstrom.logDoc (Quickstrom.logSingle Nothing (Quickstrom.prettyTrace trace'))
            case Quickstrom.reason failedTest of
              Just err -> Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
              Nothing -> pure ()
            Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
              line <> "Failed after" <+> pluralize failedAfter "test" <+> "and" <+> pluralize (Quickstrom.numShrinks failedTest) "shrink" <> "." <> line
          Quickstrom.CheckError {Quickstrom.checkError} -> do
            Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
              line <> "Check encountered an error:" <+> pretty checkError <> line
          Quickstrom.CheckSuccess {Quickstrom.passedTests} ->
            Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Green) $
              line <> "Passed" <+> pluralize (length passedTests) "test" <> "." <> line
    }
  where
    pluralize :: Int -> Doc ann -> Doc ann
    pluralize 1 term = "1" <+> term
    pluralize n term = pretty n <+> term <> "s"
