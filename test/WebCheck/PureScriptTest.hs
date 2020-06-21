{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module WebCheck.PureScriptTest where

import           Control.Lens
import           Control.Monad                       (Monad (fail))
import qualified Data.Aeson                          as JSON
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Vector                         (Vector)
import qualified Data.Vector                         as Vector
import           Language.PureScript                 (nullSourceSpan)
import           Protolude
import           System.Environment                  (lookupEnv)
import           Test.Tasty.Hspec                    hiding (Selector)
import qualified WebCheck.Element                    as WebCheck
import           WebCheck.PureScript.Eval
import           WebCheck.PureScript.Eval.Error
import           WebCheck.PureScript.ForeignFunction
import           WebCheck.PureScript.Pretty
import           WebCheck.PureScript.Program
import qualified WebCheck.PureScript.Queries         as Queries
import           WebCheck.Trace                      (ObservedState (..))

loadModules :: IO Modules
loadModules = do
  let key = "WEBCHECK_LIBRARY_DIR"
  webcheckPursDir <-
    maybe (fail (key <> " environment variable is not set")) pure
      =<< lookupEnv key
  loadLibraryModules webcheckPursDir >>= \case
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
  Program Queries.WithObservedStates ->
  Either Text b
eval' states name p =
  (toHaskellValue nullSourceSpan =<< evalWithObservedStates p name states)
  & _Left %~ (prettyText . prettyEvalError)

spec_purescript :: Spec
spec_purescript = beforeAll loadModules $ do
  describe "basics" . beforeWith (loadProgram' "test/WebCheck/PureScriptTest.purs") $ do
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
    let paragraphWithTextState :: Text -> ObservedState
        paragraphWithTextState t =
          ObservedState (HashMap.singleton "p" [HashMap.singleton (WebCheck.Property "textContent") (JSON.String t)])
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
  describe "temporal logic" . beforeWith (loadProgram' "test/WebCheck/PureScriptTest.purs") $ do
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
  describe "TodoMVC" . beforeWith (loadProgram' "specs/TodoMVC.purs") $ do
    let todoMvcState :: Text -> Text -> Text -> Vector (Text, Bool) -> ObservedState
        todoMvcState newTodo selected count todoItems =
          ( ObservedState $
              HashMap.fromList
                [ (WebCheck.Selector ".new-todo", [HashMap.singleton (WebCheck.Property "value") (JSON.String newTodo)]),
                  (WebCheck.Selector ".todoapp .filters .selected", [HashMap.singleton (WebCheck.Property "textContent") (JSON.String selected)]),
                  ( WebCheck.Selector ".todo-list li",
                     map (const (HashMap.singleton (WebCheck.CssValue "display") "block")) (Vector.toList todoItems)
                  ),
                  ( WebCheck.Selector ".todo-list li label",
                    [HashMap.singleton (WebCheck.Property "textContent") (JSON.String todo) | (todo, _) <- Vector.toList todoItems]
                  ),
                  ( WebCheck.Selector ".todo-list li input[type=checkbox]",
                    [HashMap.singleton (WebCheck.Property "checked") (JSON.Bool checked) | (_, checked) <- Vector.toList todoItems]
                  ),
                  (WebCheck.Selector ".todoapp .todo-count strong", [HashMap.singleton (WebCheck.Property "textContent") (JSON.String count)])
                ]
          )
    it "succeeds with correct states" $ \p -> do
      eval'
        [ todoMvcState "" "All" "" [],
          todoMvcState "Buy milk" "All" "" [],
          todoMvcState "" "All" "1 left" [("Buy milk", False)],
          todoMvcState "" "Active" "1 left" [("Buy milk", False)],
          todoMvcState "" "Active" "0 left" [],
          todoMvcState "" "Completed" "0 left" [("Buy milk", True)],
          todoMvcState "" "Completed" "1 left" []
        ]
        "proposition"
        p
        `shouldBe` Right True
    it "fails with incorrect initial state" $ \p -> do
      eval'
        [ todoMvcState "" "All" "1 left" [("Buy milk", False)]
        ]
        "proposition"
        p
        `shouldBe` Right False
    it "fails with incorrect action states" $ \p -> do
      eval'
        [ todoMvcState "" "All" "" [],
          todoMvcState "" "Active" "1 left" [("Buy milk", True)] -- Count of 1 even though all are checked
        ]
        "proposition"
        p
        `shouldBe` Right False
