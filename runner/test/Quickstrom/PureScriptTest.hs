{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quickstrom.PureScriptTest where

import Control.Lens
import Control.Monad (Monad (fail))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.PureScript (nullSourceSpan)
import Protolude
import qualified Quickstrom.Element as Quickstrom
import Quickstrom.PureScript.Eval
import Quickstrom.PureScript.ForeignFunction
import Quickstrom.PureScript.Pretty
import Quickstrom.PureScript.Program
import Quickstrom.Trace (ObservedState (..))
import System.Environment (lookupEnv)
import Test.Tasty.Hspec hiding (Selector)

loadModules :: IO Modules
loadModules = do
  let key = "QUICKSTROM_LIBRARY_DIR"
  quickstromPursDir <-
    maybe (fail (key <> " environment variable is not set")) pure
      =<< lookupEnv key
  loadLibraryModules quickstromPursDir >>= \case
    Right ms -> pure ms
    Left err -> fail ("Failed to load modules: " <> toS err)

loadProgram' :: Eval r m => FilePath -> Modules -> IO (Program m)
loadProgram' path modules = do
  code <- readFile path
  loadProgram modules code >>= \case
    Right p -> pure p
    Left err -> fail ("Failed to load program: " <> toS err)

eval' ::
  ToHaskellValue (Either EvalError) b =>
  [ObservedState] ->
  Text ->
  Program WithObservedStates ->
  Either Text b
eval' states name p =
  (toHaskellValue nullSourceSpan =<< evalWithObservedStates p name states)
    & _Left %~ (prettyText . prettyEvalError)

spec_purescript :: Spec
spec_purescript = beforeAll loadModules $ do
  describe "basics" . beforeWith (loadProgram' "test/Quickstrom/PureScriptTest.purs") $ do
    it "supports mutually recursive top-level bindings" $ \p -> do
      eval' [mempty] "mutuallyRecTop" p `shouldBe` Right (0 :: Int)
    it "supports mutually recursive let bindings" $ \p -> do
      eval' [mempty] "mutuallyRecLet" p `shouldBe` Right (0 :: Int)
    it "unfoldr" $ \p -> do
      eval' [mempty] "unfoldrNumbers" p `shouldBe` Right (Vector.reverse [1 .. 10 :: Int])
    it "toNumber" $ \p -> do
      eval' [mempty] "convertNum" p `shouldBe` Right (1.0 :: Double)
    it "runs state monad" $ \p -> do
      eval' [mempty] "testState" p `shouldBe` Right (0 :: Int)
    it "returns one queried element's state" $ \p -> do
      eval'
        [paragraphWithTextState "hello"]
        "testOneQuery"
        p
        `shouldBe` Right ("hello" :: Text)
    it "returns next one queried element's state" $ \p -> do
      eval'
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        "testNextOneQuery"
        p
        `shouldBe` Right ("bar" :: Text)
    describe "partial application and higher-order temporal operators" $ do
      it "supports partially applying until" $ \p -> do
        eval' @Bool
          [mempty, mempty]
          "partialUntil"
          p
          `shouldBe` Right True
      it "supports passing next as parameter" $ \p -> do
        eval'
          [mempty, mempty]
          "passNext"
          p
          `shouldBe` Right True
    it "parameters are evaluated lazily" $ \p -> do
      eval'
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        "lazyNext"
        p
        `shouldBe` Right ("bar" :: Text)
    it "parameters are evaluated lazily with nested next" $ \p -> do
      eval'
        [paragraphWithTextState "foo", paragraphWithTextState "bar", paragraphWithTextState "baz"]
        "lazyNextNext"
        p
        `shouldBe` Right ("baz" :: Text)
    it "parameters are evaluated lazily" $ \p -> do
      eval'
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        "lazyNext"
        p
        `shouldBe` Right ("bar" :: Text)
  describe "temporal logic" . beforeWith (loadProgram' "test/Quickstrom/PureScriptTest.purs") $ do
    it "tla1" $ \p -> do
      eval' [mempty, mempty] "tla1" p `shouldBe` Right True
    it "tla2" $ \p -> do
      eval' [mempty, mempty] "tla2" p `shouldBe` Right True
    it "tla3" $ \p ->
      case eval' [mempty] "tla3" p of
        Right (v :: Bool) -> expectationFailure ("Expected an error but got: " <> show v)
        Left err -> toS err `shouldContain` "cannot be determined"
    it "tla4 with 1 state" $ \p -> do
      eval' [mempty] "tla4" p `shouldBe` Right True
    it "tla4 with 2 states" $ \p -> do
      eval' [mempty, mempty] "tla4" p `shouldBe` Right True
    it "tla5" $ \p -> do
      eval' [] "tla5" p `shouldBe` Right True
    it "tla6" $ \p -> do
      eval' [mempty] "tla6" p `shouldBe` Right True
    it "tla7" $ \p -> do
      eval' [mempty] "tla7" p `shouldBe` Right True
    it "tla8" $ \p -> do
      eval' [mempty] "tla8" p `shouldBe` Right False
    it "tla9 with one state" $ \p -> do
      eval' [mempty] "tla9" p `shouldBe` Right False
    it "tla9 with two states" $ \p -> do
      eval' [mempty, mempty] "tla9" p `shouldBe` Right False
    it "tla10 with zero states" $ \p -> do
      eval'
        []
        "tla10"
        p
        `shouldBe` Right False
    it "tla10 with 3 states" $ \p -> do
      eval'
        [ paragraphWithTextState "foo",
          paragraphWithTextState "bar",
          paragraphWithTextState "baz"
        ]
        "tla10"
        p
        `shouldBe` Right True
  describe "TodoMVC" . beforeWith (loadProgram' "test/Quickstrom/PureScriptTest/TodoMVC.spec.purs") $ do
    let todoMvcState :: Text -> Text -> Text -> Vector (Text, Bool) -> [Text] -> ObservedState
        todoMvcState newTodo selected count todoItems filters =
          ( ObservedState $
              HashMap.fromList
                [ (Quickstrom.Selector ".todoapp .new-todo", [HashMap.singleton (Quickstrom.Property "value") (JSON.String newTodo)]),
                  (Quickstrom.Selector ".todoapp .filters a", [HashMap.singleton (Quickstrom.Property "textContent") (JSON.String t) | t <- filters]),
                  (Quickstrom.Selector ".todoapp .filters a.selected", [HashMap.singleton (Quickstrom.Property "textContent") (JSON.String selected)]),
                  ( Quickstrom.Selector ".todo-list li",
                    map (const (HashMap.singleton (Quickstrom.CssValue "display") "block")) (Vector.toList todoItems)
                  ),
                  ( Quickstrom.Selector ".todo-list li label",
                    [HashMap.singleton (Quickstrom.Property "textContent") (JSON.String todo) | (todo, _) <- Vector.toList todoItems]
                  ),
                  ( Quickstrom.Selector ".todo-list li input[type=checkbox]",
                    [HashMap.singleton (Quickstrom.Property "checked") (JSON.Bool checked) | (_, checked) <- Vector.toList todoItems]
                  ),
                  (Quickstrom.Selector ".todoapp .todo-count strong", [HashMap.singleton (Quickstrom.Property "textContent") (JSON.String count)])
                ]
          )
        todoFilters = ["All", "Active", "Completed"]
    it "succeeds with correct states" $ \p -> do
      eval'
        [ todoMvcState "" "All" "" [] [],
          todoMvcState "Buy milk" "All" "" [] [],
          todoMvcState "" "All" "1 left" [("Buy milk", False)] todoFilters,
          todoMvcState "" "Active" "1 left" [("Buy milk", False)] todoFilters,
          todoMvcState "" "Active" "0 left" [] todoFilters,
          todoMvcState "" "Completed" "0 left" [("Buy milk", True)] todoFilters,
          todoMvcState "" "Completed" "1 left" [] todoFilters
        ]
        "proposition"
        p
        `shouldBe` Right True
    it "fails with incorrect initial state" $ \p -> do
      eval'
        [ todoMvcState "" "All" "1 left" [("Buy milk", False)] todoFilters
        ]
        "proposition"
        p
        `shouldBe` Right False
    it "fails with incorrect action states" $ \p -> do
      eval'
        [ todoMvcState "" "All" "" [] [],
          todoMvcState "" "Active" "1 left" [("Buy milk", True)] todoFilters -- Count of 1 even though all are checked
        ]
        "proposition"
        p
        `shouldBe` Right False

paragraphWithTextState :: Text -> ObservedState
paragraphWithTextState t =
  ObservedState (HashMap.singleton "p" [HashMap.singleton (Quickstrom.Property "textContent") (JSON.String t)])
