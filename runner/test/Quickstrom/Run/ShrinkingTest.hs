module Quickstrom.Run.ShrinkingTest where

import Quickstrom.Prelude
import Quickstrom.Run.Shrinking (shrinkPrefixes, Prefix (unPrefix))
import Data.Tree
import Test.QuickCheck

prop_shrinkPrefixes_includes_all_prefixes :: NonEmptyList Int -> Property
prop_shrinkPrefixes_includes_all_prefixes (NonEmpty xs) = 
    let allPrefixes = sort (map unPrefix (foldMap flatten (shrinkPrefixes xs)))
    in allPrefixes === tailSafe (initSafe (inits xs))

prop_shrinkPrefixes_is_ordered :: NonEmptyList Int -> Property
prop_shrinkPrefixes_is_ordered (NonEmpty xs) = property $
    let treeMaxLen = foldTree (\p subLengths -> maximum (length (unPrefix p) : subLengths))
        isOrdered forest =
          let lengths = map treeMaxLen forest
          in lengths == sort lengths && all (isOrdered . subForest) forest
    in isOrdered (shrinkPrefixes xs)
        