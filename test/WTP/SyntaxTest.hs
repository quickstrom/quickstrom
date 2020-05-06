{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WTP.SyntaxTest where

import WTP.Formula.Syntax hiding ((===), property)
import WTP.Formula.Logic
import qualified WTP.Gen as Gen
import Test.QuickCheck ((.||.), (===), forAll, listOf, withMaxSuccess, counterexample, property)

prop_simple_connectives_reduce = forAll Gen.simpleConnectivesSyntax $ \s -> do
    case simplify s of
        Literal LTrue -> property True
        Literal LFalse -> property True
        s' -> counterexample (show s') (property False)
