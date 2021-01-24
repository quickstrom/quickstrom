{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Quickstrom.CLI.Version where

import Data.String (String)
import qualified Data.Version as Version
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftData, runIO)
import qualified Paths_quickstrom_cli as CLI
import Quickstrom.Prelude
import System.Environment (lookupEnv)

rev :: Q (Maybe String)
rev = runIO (lookupEnv "QUICKSTROM_GIT_REV")

version :: Q Exp
version =
  let cabalVersion = CLI.version
      version' = Version.showVersion cabalVersion
   in rev >>= \case
        Just r -> liftData (version' <> "-" <> r)
        Nothing -> liftData version'
