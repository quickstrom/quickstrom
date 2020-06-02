{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module WTP.PureScriptTest where

import Protolude
import Test.Tasty.Hspec hiding (Selector)
import WTP.PureScript
import WTP.PureScript.Value
import Language.PureScript (Ident, Qualified, nullSourceSpan)
import Language.PureScript.CoreFn
import Data.Text.Prettyprint.Doc (Pretty(pretty))
import qualified Data.Vector as Vector

eval' :: Expr EvalAnn -> Either EvalError (Value EvalAnn)
eval' = runEval . eval initialEnv

envLookupExpr :: Qualified Ident -> Eval (Expr EvalAnn)
envLookupExpr qn =
   case envLookup qn initialEnv of
       Just (Left expr) -> pure expr
       _ -> throwError (NotInScope nullSourceSpan qn)


spec_purescript :: Spec
spec_purescript = do
  it "adds integers" $ do
    let r = runEval $ do
        intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
        eval initialEnv (app (app intAdd (intLit 1)) (intLit 2))
    r `shouldSatisfy` \case
       Right (VInt 3) -> True
       _ -> False
  it "maps over array" $ do
    let r :: Either EvalError (Value EvalAnn)
        r = runEval $ do
                arrayMap <- envLookupExpr (qualifiedName ["Data", "Functor"] "arrayMap")
                intAdd <- envLookupExpr (qualifiedName ["Data", "Semiring"] "intAdd")
                let incr = app intAdd (intLit 1)
                let incrAll = (app arrayMap incr)
                -- traceShowM (const () <$> incrAll)
                eval initialEnv (app incrAll (arrayLit [intLit 1, intLit 2, intLit 3]))
    prettyText (either pretty pretty r) `shouldBe` "[2, 3, 4]"
  it "evaluates TodoMVC" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScript", "TodoMVC"] "angularjs") `shouldReturn` Right True

  it "supports mutually recursive top-level bindings" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecTop") `shouldReturn` Right (0 :: Int)

  it "supports mutually recursive let bindings" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScriptTest"] "mutuallyRecLet") `shouldReturn` Right (0 :: Int)

  it "unfoldr" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScriptTest"] "unfoldrNumbers") `shouldReturn` Right (Vector.reverse [1..10 :: Int])

  it "toNumber" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScriptTest"] "convertNum") `shouldReturn` Right (1.0 :: Double)

  it "runs state monad" $ do
    runWithEntryPoint (qualifiedName ["WTP", "PureScriptTest"] "testState") `shouldReturn` Right (Vector.reverse [1..10 :: Int])


nullAnn :: EvalAnn
nullAnn = (EvalAnn nullSourceSpan Nothing Nothing)

app :: Expr EvalAnn -> Expr EvalAnn -> Expr EvalAnn
app = App nullAnn

intLit :: Integer -> Expr EvalAnn
intLit n = Literal nullAnn (NumericLiteral (Left n))

arrayLit :: [Expr EvalAnn] -> Expr EvalAnn
arrayLit xs = Literal nullAnn (ArrayLiteral xs)