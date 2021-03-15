{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Shrinking (Prefix (..), shrinkPrefixes, searchSmallestFailingPrefix, drawShrinkForest) where

import Control.Lens
import Control.Lens.Extras (is)
import Data.String (String)
import Data.Tree
import qualified Pipes
import Quickstrom.Prelude hiding (Prefix)

newtype Prefix a = Prefix { unPrefix :: [a] }
  deriving (Eq, Show, Hashable, Functor, Foldable, Traversable)

shrinkPrefixes :: [a] -> Forest (Prefix a)
shrinkPrefixes as = go 0 (length as `div` 2) as
  where
    go minLength n xs
      | n < 1 = []
      | otherwise =
        let len = length xs - n
        in if len <= minLength
          then go minLength (n `div` 2) xs
          else
            let xs' = take len xs
            in Node (Prefix xs') (go minLength (len `div` 2) xs') :
                go len (n `div` 2) xs

searchSmallestFailingPrefix :: Monad m => (Prefix a -> m b) -> Prism' b x -> Forest (Prefix a) -> Pipes.Producer b m ()
searchSmallestFailingPrefix test failure = go
  where
    go = \case
      [] -> pure ()
      Node x xs : rest -> do
        r <- lift (test x)
        Pipes.yield r
        if is failure r
          then go xs
          else go rest

drawShrinkForest :: Show a => Forest (Prefix a) -> String
drawShrinkForest = drawForest . map (map show)