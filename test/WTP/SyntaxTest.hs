{-# LANGUAGE ScopedTypeVariables #-}
module WTP.SyntaxTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Syntax as Syntax

hprop_to_nnf :: Property
hprop_to_nnf = property $ do
  s <- forAll genSyntax
  let nnf = Syntax.toNNF s
  annotateShow nnf
  assert (NNF.depth nnf < 5)
  -- assert (NNF.depth nnf <= Syntax.depth s)
  pure ()

genSyntax :: Gen (Syntax.FormulaWith ())
genSyntax =
  Gen.small $
    Gen.recursive
      Gen.choice
      [ pure Syntax.True,
        pure Syntax.False,
        pure (Syntax.Assert ())
      ]
      [ Gen.subterm genSyntax Syntax.Not,
        Gen.subterm2 genSyntax genSyntax Syntax.And,
        Gen.subterm2 genSyntax genSyntax Syntax.Or,
        Gen.subterm2 genSyntax genSyntax Syntax.Until,
        Gen.subterm genSyntax Syntax.Always,
        Gen.subterm genSyntax Syntax.Eventually
      ]

main = Main.defaultMain [check hprop_to_nnf]