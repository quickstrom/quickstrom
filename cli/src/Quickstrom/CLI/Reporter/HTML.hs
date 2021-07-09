{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Quickstrom.CLI.Reporter.HTML where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Labels ()
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.CLI.Reporter.JSON as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (State, to, uncons)
import qualified System.Directory as Directory
import System.Environment (getEnv)
import System.FilePath ((</>))

newtype HTMLReporterException = HTMLReporterException Text
  deriving (Show, Eq)

instance Exception HTMLReporterException

htmlReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => FilePath -> Quickstrom.Reporter m
htmlReporter reportDir = Quickstrom.Reporter {preCheck, report}
  where
    jsonReporter = Quickstrom.jsonReporter reportDir
    preCheck = Quickstrom.preCheck jsonReporter
    report webDriverOpts checkOpts result = do
      Quickstrom.report jsonReporter webDriverOpts checkOpts result
      liftIO $ do
        json <- LBS.readFile (reportDir </> "report.json")
        LBS.writeFile (reportDir </> "report.jsonp.js") ("window.report = " <> json)
        getAssets >>= \case
          Just assets | not (null assets) ->
            for_ assets $ \(name, contents) ->
              BS.writeFile (reportDir </> name) contents
          _ -> throwIO (HTMLReporterException "HTML report assets not found.")

getAssets :: MonadIO m => m (Maybe [(FilePath, ByteString)])
getAssets = liftIO $ do
  path <- getEnv "QUICKSTROM_HTML_REPORT_DIR"
  files <-
    Directory.listDirectory path
      >>= traverse (\fileName -> (fileName,) <$> BS.readFile (path </> fileName))
  pure (Just files)
