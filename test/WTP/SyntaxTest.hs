{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WTP.SyntaxTest where

import WTP.Formula
import qualified WTP.Gen as Gen
import Test.QuickCheck hiding ((===))
import qualified Data.Aeson as JSON

prop_simple_connectives_reduce :: Property
prop_simple_connectives_reduce = forAll Gen.simpleConnectivesSyntax $ \s -> do
    case simplify s of
        Literal JSON.Bool{} -> property True
        s' -> counterexample (show s') (property False)
