module WTP.Gen where

import qualified Data.Bool as Bool
import Test.QuickCheck
import WTP.Formula.Syntax
import Prelude hiding (Bool (..))

variable :: Gen Char
variable = arbitraryASCIIChar

trace :: Gen [String]
trace = listOf (listOf variable)

nonEmpty :: Gen [a] -> Gen [a]
nonEmpty = flip suchThat (Prelude.not . null)

anySyntax :: Gen (FormulaWith Char)
anySyntax = sized syntax'
  where
    syntax' :: Int -> Gen (FormulaWith Char)
    syntax' 0 =
      oneof
        [ pure True,
          pure False,
          Assert <$> variable
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ Not <$> subterm,
              And <$> subterm <*> subterm,
              Or <$> subterm <*> subterm,
              Until <$> subterm <*> subterm,
              Implies <$> subterm <*> subterm,
              Equivalent <$> subterm <*> subterm,
              Always <$> subterm,
              Eventually <$> subterm,
              Next <$> subterm
            ]

trueSyntax :: Gen Char -> Gen (FormulaWith Char)
trueSyntax genVariable = sized syntax'
  where
    syntax' :: Int -> Gen (FormulaWith Char)
    syntax' 0 =
      oneof
        [ pure True,
          Assert <$> genVariable
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ And <$> subterm <*> subterm,
              Or <$> subterm <*> subterm,
              Until <$> subterm <*> subterm,
              Implies <$> subterm <*> subterm,
              Equivalent <$> subterm <*> subterm,
              Always <$> subterm,
              Eventually <$> subterm,
              Next <$> subterm
            ]

falseSyntax :: Gen Char -> Gen (FormulaWith Char)
falseSyntax genVariable = sized syntax'
  where
    syntax' :: Int -> Gen (FormulaWith Char)
    syntax' 0 =
      oneof
        [ pure False,
          Assert <$> genVariable
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ And <$> subterm <*> subterm,
              Or <$> subterm <*> subterm,
              Until <$> subterm <*> subterm,
              Always <$> subterm,
              Eventually <$> subterm,
              Next <$> subterm
            ]
