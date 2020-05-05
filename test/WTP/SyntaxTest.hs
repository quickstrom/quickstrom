{-# LANGUAGE ScopedTypeVariables #-}
module WTP.SyntaxTest where

import WTP.Formula.Syntax hiding ((===))
import qualified WTP.Gen as Gen
import Prelude hiding (Bool (..), not)
import Test.QuickCheck ((.||.), (===), forAll, listOf, withMaxSuccess)

prop_simple_connectives_reduce = forAll Gen.simpleConnectivesSyntax $ \s -> do
    let nnf = toNNF s
    nnf === NNF.True .||. nnf === NNF.False 
