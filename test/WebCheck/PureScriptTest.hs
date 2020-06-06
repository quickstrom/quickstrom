{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScriptTest where

import qualified Data.HashMap.Strict as HashMap
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Language.PureScript (Ident, Qualified, nullSourceSpan)
import Language.PureScript.CoreFn
import Protolude
import System.Process (callCommand)
import Test.Tasty.Hspec hiding (Selector)
import qualified WebCheck.Element as WebCheck
import WebCheck.PureScript
import WebCheck.PureScript.Value
import qualified WebCheck.Query as WebCheck
import qualified WebCheck.Trace as WebCheck
import qualified WebCheck.Value as WebCheck

envLookupExpr :: Qualified Ident -> Eval (Expr EvalAnn)
envLookupExpr qn =
  case envLookup qn initialEnv of
    Just (Left expr) -> pure expr
    _ -> throwError (NotInScope nullSourceSpan qn)

loadProgram' :: IO Program
loadProgram' = do
  callCommand "spago build -u '-g corefn'"
  fromRight undefined <$> loadProgram

spec_purescript :: Spec
spec_purescript = beforeAll loadProgram' $ do
  describe "basics" $ do
    it "adds integers" $ \p -> do
      let r = runEval [mempty] $ do
            intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
            eval initialEnv (app (app intAdd (intLit 1)) (intLit 2))
      r `shouldSatisfy` \case
        Right (VInt 3) -> True
        _ -> False
    it "maps over array" $ \p -> do
      let r :: Either EvalError (Value EvalAnn)
          r = runEval [mempty] $ do
            arrayMap <- envLookupExpr (qualifiedName ["Data", "Functor"] "arrayMap")
            intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
            let incr = app intAdd (intLit 1)
            let incrAll = (app arrayMap incr)
            eval initialEnv (app incrAll (arrayLit [intLit 1, intLit 2, intLit 3]))
      prettyText (either pretty pretty r) `shouldBe` "[2, 3, 4]"
    it "supports mutually recursive top-level bindings" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "mutuallyRecTop") p `shouldReturn` Right (0 :: Int)
    it "supports mutually recursive let bindings" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "mutuallyRecLet") p `shouldReturn` Right (0 :: Int)
    it "unfoldr" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "unfoldrNumbers") p `shouldReturn` Right (Vector.reverse [1 .. 10 :: Int])
    it "toNumber" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "convertNum") p `shouldReturn` Right (1.0 :: Double)
    it "runs state monad" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "testState") p `shouldReturn` Right (0 :: Int)
    let paragraphWithTextState :: Text -> QueriedElements
        paragraphWithTextState t =
          HashMap.singleton "p" [HashMap.singleton (WebCheck.Property "textContent") (VString t)]
    it "returns one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "hello"]
        (qualifiedName ["WebCheck", "PureScriptTest"] "testOneQuery")
        p
        `shouldReturn` Right ("hello" :: Text)
    it "returns next one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        (qualifiedName ["WebCheck", "PureScriptTest"] "testNextOneQuery")
        p
        `shouldReturn` Right ("bar" :: Text)
  describe "temporal logic" $ do
    it "tla1" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "tla1") p `shouldReturn` Right True
    it "tla2" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "tla2") p `shouldReturn` Right True
    it "tla3" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WebCheck", "PureScriptTest"] "tla3") p >>= \case
        Right (v :: Bool) -> expectationFailure ("Expected an error but got: " <> show v)
        Left msg -> toS msg `shouldContain` "cannot be determined"
  describe "TodoMVC" $ do
    let todoMvcState :: Text -> Text -> Text -> Vector (Text, Bool) -> QueriedElements
        todoMvcState newTodo selected count todoItems =
          ( HashMap.fromList
              [ (WebCheck.Selector ".new-todo", [HashMap.singleton (WebCheck.Property "value") (VString newTodo)]),
                (WebCheck.Selector ".todoapp .filters .selected", [HashMap.singleton (WebCheck.Property "textContent") (VString selected)]),
                ( WebCheck.Selector ".todo-list li",
                  [HashMap.singleton (WebCheck.Property "textContent") (VString todo) | (todo, _) <- Vector.toList todoItems]
                ),
                ( WebCheck.Selector ".todo-list li input[type=checkbox]",
                  [HashMap.singleton (WebCheck.Property "checked") (VBool checked) | (_, checked) <- Vector.toList todoItems]
                ),
                (WebCheck.Selector ".todoapp .todo-count strong", [HashMap.singleton (WebCheck.Property "textContent") (VString count)])
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
        (qualifiedName ["WebCheck", "PureScript", "TodoMVC"] "angularjs")
        p
        `shouldReturn` Right True
    it "fails with incorrect initial state" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "1 left" [("Buy milk", False)],
          mempty
        ]
        (qualifiedName ["WebCheck", "PureScript", "TodoMVC"] "angularjs")
        p
        `shouldReturn` Right False
    it "fails with incorrect action states" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "" [],
          todoMvcState "" "Active" "1 left" [("Buy milk", True)] -- Count of 1 even though all are checked
        ]
        (qualifiedName ["WebCheck", "PureScript", "TodoMVC"] "angularjs")
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
