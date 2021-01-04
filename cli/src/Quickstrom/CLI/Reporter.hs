{-# LANGUAGE MultiParamTypeClasses #-}

module Quickstrom.CLI.Reporter where

import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.WebDriver.Class as Quickstrom

type Reporter m =
  Quickstrom.WebDriverOptions ->
  Quickstrom.CheckOptions ->
  Quickstrom.CheckResult ->
  m ()
