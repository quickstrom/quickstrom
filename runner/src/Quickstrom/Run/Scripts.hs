{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Scripts where

import Control.Monad (Monad (fail))
import qualified Data.Aeson as JSON
import Data.String (String, fromString)
import Quickstrom.Element (Element)
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Specification (Queries)
import Quickstrom.Timeout (Timeout)
import Quickstrom.Trace (ObservedElementStates)
import Quickstrom.WebDriver.Class (WebDriver, runScript)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

newtype CheckScript a = CheckScript {runCheckScript :: forall m. WebDriver m => m a}

data CheckScripts = CheckScripts
  { isElementVisible :: Element -> CheckScript Bool,
    observeState :: Queries -> CheckScript ObservedElementStates,
    registerNextStateObserver :: Timeout -> Queries -> CheckScript (),
    awaitNextState :: CheckScript ()
  }

readScripts :: MonadIO m => m CheckScripts
readScripts = do
  let key = "QUICKSTROM_CLIENT_SIDE_DIR"
  dir <- liftIO (maybe (fail (key <> " environment variable not set")) pure =<< lookupEnv key)
  let readScript :: MonadIO m => String -> m Text
      readScript name = liftIO (fromString . toS <$> readFile (dir </> name <> ".js"))
  isElementVisibleScript <- readScript "isElementVisible"
  observeStateScript <- readScript "observeState"
  registerNextStateObserverScript <- readScript "registerNextStateObserver"
  awaitNextStateScript <- readScript "awaitNextState"
  pure
    CheckScripts
      { isElementVisible = \el -> CheckScript ((== JSON.Bool True) <$> runScript isElementVisibleScript [JSON.toJSON el]),
        observeState = \queries' -> CheckScript (runScript observeStateScript [JSON.toJSON queries']),
        registerNextStateObserver = \timeout queries' -> CheckScript (runScript registerNextStateObserverScript [JSON.toJSON timeout, JSON.toJSON queries']),
        awaitNextState = CheckScript (runScript awaitNextStateScript [])
      }
