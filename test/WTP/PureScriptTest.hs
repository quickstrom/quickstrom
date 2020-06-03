{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

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

oneEmptyState :: [WTP.ObservedState]
oneEmptyState = [mempty]

spec_purescript :: Spec
spec_purescript = beforeAll loadProgram' $ do
  it "adds integers" $ \p -> do
    let r = runEval oneEmptyState $ do
          intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
          eval initialEnv (app (app intAdd (intLit 1)) (intLit 2))
    r `shouldSatisfy` \case
      Right (VInt 3) -> True
      _ -> False
  it "maps over array" $ \p -> do
    let r :: Either EvalError (Value EvalAnn)
        r = runEval oneEmptyState $ do
          arrayMap <- envLookupExpr (qualifiedName ["Data", "Functor"] "arrayMap")
          intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
          let incr = app intAdd (intLit 1)
          let incrAll = (app arrayMap incr)
          eval initialEnv (app incrAll (arrayLit [intLit 1, intLit 2, intLit 3]))
    prettyText (either pretty pretty r) `shouldBe` "[2, 3, 4]"
  it "evaluates TodoMVC" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScript", "TodoMVC"] "angularjs") p `shouldReturn` Right True
  it "supports mutually recursive top-level bindings" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecTop") p `shouldReturn` Right (0 :: Int)
  it "supports mutually recursive let bindings" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecLet") p `shouldReturn` Right (0 :: Int)
  it "unfoldr" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScriptTest"] "unfoldrNumbers") p `shouldReturn` Right (Vector.reverse [1 .. 10 :: Int])
  it "toNumber" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScriptTest"] "convertNum") p `shouldReturn` Right (1.0 :: Double)
  it "runs state monad" $ \p -> do
    runWithEntryPoint oneEmptyState (qualifiedName ["WTP", "PureScriptTest"] "testState") p `shouldReturn` Right (0 :: Int)
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

nullAnn :: EvalAnn
nullAnn = (EvalAnn nullSourceSpan Nothing Nothing)

app :: Expr EvalAnn -> Expr EvalAnn -> Expr EvalAnn
app = App nullAnn

intLit :: Integer -> Expr EvalAnn
intLit n = Literal nullAnn (NumericLiteral (Left n))

arrayLit :: [Expr EvalAnn] -> Expr EvalAnn
arrayLit xs = Literal nullAnn (ArrayLiteral xs)
