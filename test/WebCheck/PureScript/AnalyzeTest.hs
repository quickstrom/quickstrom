{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.PureScript.AnalyzeTest where

import Control.Monad (Monad (fail))
import Protolude
import System.Environment (lookupEnv)
import Test.Tasty.Hspec hiding (Selector)
import WebCheck.Element
import WebCheck.PureScript.Program

loadModules :: IO Modules
loadModules = do
  let key = "WEBCHECK_LIBRARY_DIR"
  webcheckPursDir <-
    maybe (fail (key <> " environment variable is not set")) pure
      =<< lookupEnv key
  loadLibraryModules webcheckPursDir >>= \case
    Right ms -> pure ms
    Left err -> fail ("Failed to load modules: " <> toS err)

loadSpecificationProgram' :: FilePath -> Modules -> IO SpecificationProgram
loadSpecificationProgram' path modules = do
  code <- readFile path
  loadSpecification modules code >>= \case
    Right p -> pure p
    Left err -> fail ("Failed to load specification program: " <> toS err)

spec_analyze :: Spec
spec_analyze = beforeAll loadModules $ do
  it "extracts all queries when valid" $ \m -> do
    s <- loadSpecificationProgram' "test/WebCheck/PureScript/AnalyzeTest/Valid.purs" m
    specificationQueries s
      `shouldBe` [ ("p", [CssValue "display"]),
                   ("button", [Property "textContent"])
                 ]
