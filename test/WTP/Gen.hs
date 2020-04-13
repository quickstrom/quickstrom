module WTP.Gen where

import qualified Data.Bool as Bool
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import WTP.Formula.Syntax
import Prelude hiding (Bool (..))

variable :: Gen Char
variable = Gen.alpha

trace :: Range Int -> Gen [String]
trace range = Gen.list range (Gen.string (Range.linear 1 3) variable)

anySyntax :: Gen (FormulaWith Char)
anySyntax =
  Gen.small $
    Gen.recursive
      Gen.choice
      [ pure True,
        pure False,
        Assert <$> variable
      ]
      [ Gen.subterm anySyntax Not,
        Gen.subterm2 anySyntax anySyntax And,
        Gen.subterm2 anySyntax anySyntax Or,
        Gen.subterm2 anySyntax anySyntax Until,
        Gen.subterm2 anySyntax anySyntax Implies,
        Gen.subterm2 anySyntax anySyntax Equivalent,
        Gen.subterm anySyntax Always,
        Gen.subterm anySyntax Eventually
      ]

trueSyntax :: Gen Char -> Gen (FormulaWith Char)
trueSyntax genVariable = gen
  where
    gen =
      Gen.recursive
        Gen.choice
        [ pure True,
          Assert <$> genVariable
        ]
        [ Gen.subterm2 gen gen And,
          Gen.subterm2 gen gen Or,
          Gen.subterm2 gen gen Until,
          Gen.subterm2 gen gen Implies,
          Gen.subterm2 gen gen Equivalent,
          Gen.subterm gen Always,
          Gen.subterm gen Eventually
        ]

falseSyntax :: Gen Char -> Gen (FormulaWith Char)
falseSyntax genVariable = gen
  where
    gen =
      Gen.recursive
        Gen.choice
        [ pure False,
          Assert <$> genVariable
        ]
        [ Gen.subterm2 gen gen Or,
          Gen.subterm2 gen gen And,
          Gen.subterm2 gen gen Until,
          Gen.subterm gen Always,
          Gen.subterm gen Eventually
        ]
