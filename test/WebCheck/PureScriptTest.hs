{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScriptTest where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Aeson as JSON
import Data.Vector (Vector)
import Language.PureScript (Ident, Qualified, nullSourceSpan)
import Language.PureScript.CoreFn
import Protolude
import Protolude.Error (error)
import System.Environment.Blank (getEnv)
import Test.Tasty.Hspec hiding (Selector)
import qualified WebCheck.Element as WebCheck
import WebCheck.Trace (ObservedState (..))
import WebCheck.PureScript
import WebCheck.PureScript.Value

envLookupExpr :: Qualified Ident -> Eval (Expr EvalAnn)
envLookupExpr qn =
  case envLookup qn initialEnv of
    Just (Left expr) -> pure expr
    _ -> throwError (NotInScope nullSourceSpan qn)

loadModules :: IO Modules
loadModules = do
  webcheckPursDir <- fromMaybe "." <$> getEnv "PURESCRIPT_WEBCHECK"
  loadLibraryModules webcheckPursDir >>= \case
    Right ms -> pure ms
    Left err -> error ("Failed to load modules: " <> err)

loadProgram' :: FilePath -> Modules -> IO Program
loadProgram' path modules = do
  code <- readFile path
  loadProgram modules code >>= \case
    Right p -> pure p
    Left err -> error ("Failed to load program: " <> err)

spec_purescript :: Spec
spec_purescript = beforeAll loadModules $ do
  describe "basics" . beforeWith (loadProgram' "test/WebCheck/PureScriptTest.purs") $ do
    it "supports mutually recursive top-level bindings" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "mutuallyRecTop") p `shouldReturn` Right (0 :: Int)
    it "supports mutually recursive let bindings" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "mutuallyRecLet") p `shouldReturn` Right (0 :: Int)
    it "unfoldr" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "unfoldrNumbers") p `shouldReturn` Right (Vector.reverse [1 .. 10 :: Int])
    it "toNumber" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "convertNum") p `shouldReturn` Right (1.0 :: Double)
    it "runs state monad" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "testState") p `shouldReturn` Right (0 :: Int)
    let paragraphWithTextState :: Text -> ObservedState
        paragraphWithTextState t =
          ObservedState (HashMap.singleton "p" [HashMap.singleton (WebCheck.Property "textContent") (JSON.String t)])
    it "returns one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "hello"]
        (qualifiedName "WebCheck.PureScriptTest" "testOneQuery")
        p
        `shouldReturn` Right ("hello" :: Text)
    it "returns next one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        (qualifiedName "WebCheck.PureScriptTest" "testNextOneQuery")
        p
        `shouldReturn` Right ("bar" :: Text)
  describe "temporal logic" . beforeWith (loadProgram' "test/WebCheck/PureScriptTest.purs") $ do
    it "tla1" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName "WebCheck.PureScriptTest" "tla1") p `shouldReturn` Right True
    it "tla2" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName "WebCheck.PureScriptTest" "tla2") p `shouldReturn` Right True
    it "tla3" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName "WebCheck.PureScriptTest" "tla3") p >>= \case
        Right (v :: Bool) -> expectationFailure ("Expected an error but got: " <> show v)
        Left msg -> toS msg `shouldContain` "cannot be determined"
  describe "TodoMVC" . beforeWith (loadProgram' "specs/TodoMVC.purs") $ do
    let todoMvcState :: Text -> Text -> Text -> Vector (Text, Bool) -> ObservedState
        todoMvcState newTodo selected count todoItems =
          ( ObservedState $ HashMap.fromList
              [ (WebCheck.Selector ".new-todo", [HashMap.singleton (WebCheck.Property "value") (JSON.String newTodo)]),
                (WebCheck.Selector ".todoapp .filters .selected", [HashMap.singleton (WebCheck.Property "textContent") (JSON.String selected)]),
                ( WebCheck.Selector ".todo-list li",
                  [HashMap.singleton (WebCheck.Property "textContent") (JSON.String todo) | (todo, _) <- Vector.toList todoItems]
                ),
                ( WebCheck.Selector ".todo-list li input[type=checkbox]",
                  [HashMap.singleton (WebCheck.Property "checked") (JSON.Bool checked) | (_, checked) <- Vector.toList todoItems]
                ),
                (WebCheck.Selector ".todoapp .todo-count strong", [HashMap.singleton (WebCheck.Property "textContent") (JSON.String count)])
              ]
          )
    it "succeeds with correct states" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "" [],
          todoMvcState "Buy milk" "All" "" [],
          todoMvcState "" "All" "1 left" [("Buy milk", False)],
          todoMvcState "" "Active" "1 left" [("Buy milk", False)],
          todoMvcState "" "Active" "0 left" [],
          todoMvcState "" "Completed" "0 left" [("Buy milk", True)],
          todoMvcState "" "Completed" "1 left" []
        ]
        (qualifiedName "WebCheck.PureScript.TodoMVC" "angularjs")
        p
        `shouldReturn` Right True
    it "fails with incorrect initial state" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "1 left" [("Buy milk", False)]
        ]
        (qualifiedName "WebCheck.PureScript.TodoMVC" "angularjs")
        p
        `shouldReturn` Right False
    it "fails with incorrect action states" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "" [],
          todoMvcState "" "Active" "1 left" [("Buy milk", True)] -- Count of 1 even though all are checked
        ]
        (qualifiedName "WebCheck.PureScript.TodoMVC" "angularjs")
        p
        `shouldReturn` Right False

nullAnn :: EvalAnn
nullAnn = (EvalAnn nullSourceSpan Nothing Nothing)

app :: Expr EvalAnn -> Expr EvalAnn -> Expr EvalAnn
app = App nullAnn

intLit :: Integer -> Expr EvalAnn
intLit n = Literal nullAnn (NumericLiteral (Left n))

arrayLit :: [Expr EvalAnn] -> Expr EvalAnn
arrayLit xs = Literal nullAnn (ArrayLiteral xs)
