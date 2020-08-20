{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.PureScript.AnalyzeTest where

import Control.Monad (Monad (fail))
import System.Environment (lookupEnv)
import Test.Tasty.Hspec hiding (Selector)
import Quickstrom.Element
import Quickstrom.Prelude
import Quickstrom.PureScript.Program

loadModules :: IO Modules
loadModules = do
  let key = "WEBCHECK_LIBRARY_DIR"
  quickstromPursDir <-
    maybe (fail (key <> " environment variable is not set")) pure
      =<< lookupEnv key
  loadLibraryModules quickstromPursDir >>= \case
    Right ms -> pure ms
    Left err -> fail ("Failed to load modules: " <> toS err)

loadSpecificationProgram' :: FilePath -> Modules -> IO (Either Text SpecificationProgram)
loadSpecificationProgram' path modules = do
  code <- readFile path
  loadSpecification modules code

spec_analyze :: Spec
spec_analyze = beforeAll loadModules $ do
  describe "extracts all queries when" $ do
    it "valid" $ \m -> do
      fmap specificationQueries <$> loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/Valid.purs" m
        `shouldReturn` Right
          [ ("p", [CssValue "display"]),
            ("button", [Property "textContent"]),
            ("a", [Property "textContent"])
          ]
    it "valid and using top-level element state" $ \m -> do
      fmap specificationQueries <$> loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/TopLevelElementState.purs" m
        `shouldReturn` Right [("p", [CssValue "display", CssValue "font-size"])]
  describe "rejects the specification when" $ do
    it "using queries with identifiers bound in let" $ \m -> do
      Left err <- loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/FreeVariablesLocal.purs" m
      toS err `shouldContain` "Unsupported query expression"
    it "using queries with identifiers bound at top level" $ \m -> do
      Left err <- loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/FreeVariablesTopLevel.purs" m
      toS err `shouldContain` "Unsupported query expression"
    it "constructing queries from results of other queries bound in let" $ \m -> do
      Left err <- loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/DependentQueryLocal.purs" m
      toS err `shouldContain` "Unsupported query expression"
    it "constructing queries from results of other queries bound at top level" $ \m -> do
      Left err <- loadSpecificationProgram' "test/Quickstrom/PureScript/AnalyzeTest/DependentQueryTopLevel.purs" m
      toS err `shouldContain` "Unsupported query expression"
