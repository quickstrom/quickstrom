{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WTP.PureScriptTest where

import qualified Data.HashMap.Strict as HashMap
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import qualified Data.Vector as Vector
import Language.PureScript (Ident, Qualified, nullSourceSpan)
import Language.PureScript.CoreFn
import Protolude
import System.Process (callCommand)
import Test.Tasty.Hspec hiding (Selector)
import qualified WTP.Element as WTP
import WTP.PureScript
import WTP.PureScript.Value
import qualified WTP.Query as WTP
import qualified WTP.Trace as WTP
import qualified WTP.Value as WTP

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
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecTop") p `shouldReturn` Right (0 :: Int)
    it "supports mutually recursive let bindings" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecLet") p `shouldReturn` Right (0 :: Int)
    it "unfoldr" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "unfoldrNumbers") p `shouldReturn` Right (Vector.reverse [1 .. 10 :: Int])
    it "toNumber" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "convertNum") p `shouldReturn` Right (1.0 :: Double)
    it "runs state monad" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "testState") p `shouldReturn` Right (0 :: Int)
    let paragraphWithTextState t =
          WTP.ObservedState
            ( HashMap.singleton
                (WTP.Get (WTP.Property "textContent") (WTP.ByCss "p"))
                [WTP.VString t]
            )
    it "returns one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "hello"]
        (qualifiedName ["WTP", "PureScriptTest"] "testOneQuery")
        p
        `shouldReturn` Right ("hello" :: Text)
    it "returns next one queried element's state" $ \p -> do
      runWithEntryPoint
        [paragraphWithTextState "foo", paragraphWithTextState "bar"]
        (qualifiedName ["WTP", "PureScriptTest"] "testNextOneQuery")
        p
        `shouldReturn` Right ("bar" :: Text)

  describe "temporal logic" $ do
    it "tla1" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName ["WTP", "PureScriptTest"] "tla1") p `shouldReturn` Right True
    it "tla2" $ \p -> do
      runWithEntryPoint [mempty, mempty] (qualifiedName ["WTP", "PureScriptTest"] "tla2") p `shouldReturn` Right True
    it "tla3" $ \p -> do
      runWithEntryPoint [mempty] (qualifiedName ["WTP", "PureScriptTest"] "tla3") p >>= \case
        Right (v :: Bool) -> expectationFailure ("Expected an error but got: " <> show v)
        Left msg -> toS msg `shouldContain` "cannot be determined"

  describe "TodoMVC" $ do
    let todoMvcState newTodo selected count todoItems =
          WTP.ObservedState
            ( HashMap.fromList
                [ (WTP.Get (WTP.Property "value") (WTP.ByCss ".new-todo"), [WTP.VString newTodo]),
                  (WTP.Get (WTP.Property "textContent") (WTP.ByCss ".todoapp .filters .selected"), [WTP.VString selected]),
                  (WTP.Get (WTP.Property "textContent") (WTP.ByCss ".todo-list li"), (map (WTP.VString . fst) todoItems)),
                  (WTP.Get (WTP.Property "checked") (WTP.ByCss ".todo-list li input[type=checkbox]"), (map (WTP.VBool . snd) todoItems)),
                  (WTP.Get (WTP.Property "textContent") (WTP.ByCss ".todoapp .todo-count strong"), [WTP.VString count])
                ]
            )
    it "succeeds with correct states" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "0" [],
          todoMvcState "Buy milk" "All" "0" [],
          todoMvcState "" "All" "1" [("Buy milk", False)],
          todoMvcState "" "All" "0" [("Buy milk", True)]
        ]
        (qualifiedName ["WTP", "PureScript", "TodoMVC"] "angularjs")
        p
        `shouldReturn` Right True

    it "fails with incorrect action states" $ \p -> do
      runWithEntryPoint
        [ todoMvcState "" "All" "0" [],
          todoMvcState "Buy milk" "All" "0" [],
          todoMvcState "" "All" "1" [("Buy milk", True)] -- Count of 1 even though all are checked
        ]
        (qualifiedName ["WTP", "PureScript", "TodoMVC"] "angularjs")
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
