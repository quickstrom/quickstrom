{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Quickstrom.Run.Shrinking where

import Quickstrom.Prelude

import qualified Pipes
import Data.Tree
import Control.Lens
import Control.Lens.Extras (is)

newtype Shrink a = Shrink a
  deriving (Eq, Show)

shrinkForest :: (a -> [a]) -> a -> Forest (Shrink a)
shrinkForest shrink = go
  where
    go = map (\x -> Node (Shrink x) (go x)) . shrink

shrinkActions :: [a] -> [[a]]
shrinkActions [] = []
shrinkActions as = filter (not . null) ([first75] <> maybeToList (initMay as))
  where
    first75 = take (floor @Double (fromIntegral (length as) * 0.75)) as

traverseShrinks :: Monad m => (Shrink a -> m b) -> Prism' b x -> Forest (Shrink a) -> Pipes.Producer b m ()
traverseShrinks test failure = go
  where
    go = \case
      [] -> pure ()
      Node x xs : rest -> do
        r <- lift (test x)
        Pipes.yield r
        when (is failure r) do
          go xs
        go rest