{-# LANGUAGE ScopedTypeVariables #-}
module WTP.SyntaxTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_is_ok :: Property
hprop_is_ok = property $ do
    (x :: Int) <- forAll (Gen.integral (Range.linear 0 10))
    x + x === x * 2
