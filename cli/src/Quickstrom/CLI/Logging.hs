{-# LANGUAGE FlexibleContexts #-}

module Quickstrom.CLI.Logging where

import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude

logSingle :: Maybe Quickstrom.LogLevel -> Doc AnsiStyle -> [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)]
logSingle l d = [(l, d)]

logDoc :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)] -> m ()
logDoc logs = for_ logs $ \(logLevel, doc) -> do
  minLogLevel <- ask
  let logAction = putStrLn (renderStrict (layoutPretty defaultLayoutOptions doc))
  case logLevel of
    Just level
      | level >= minLogLevel -> logAction
      | otherwise -> pass
    Nothing -> logAction
