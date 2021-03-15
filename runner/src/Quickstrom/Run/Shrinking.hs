{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Shrinking where

import Control.Lens
import Control.Lens.Extras (is)
import Data.String (String)
import Data.Tree
import qualified Pipes
import Quickstrom.Prelude

newtype Shrink a = Shrink a
  deriving (Eq, Show)

shrinkForest :: (Ord strategy, Enum strategy, Bounded strategy) => (strategy -> a -> [a]) -> a -> Forest (Shrink a)
shrinkForest shrink = go [minBound .. maxBound]
  where
    go [] _ = []
    go strategies x =
      [ Node (Shrink x') (go (filter (>= strategy) strategies) x')
        | strategy <- strategies,
          x' <- shrink strategy x
      ]

data ListStrategy = Half | Quarter | Single
  deriving (Eq, Show, Ord, Enum, Bounded)

shrinkActions :: ListStrategy -> [a] -> [[a]]
shrinkActions _ [] = []
shrinkActions strategy as = filter (not . null) $ case strategy of
  Half -> [firstPercent 0.5 as]
  Quarter -> [firstPercent 0.75 as]
  Single -> maybeToList (initMay as)
  where
    firstPercent :: Double -> [a] -> [a]
    firstPercent p = take (floor (fromIntegral (length as) * p))

traverseShrinks :: Monad m => (Shrink a -> m b) -> Prism' b x -> Forest (Shrink a) -> Pipes.Producer b m ()
traverseShrinks test failure = go
  where
    go = \case
      [] -> pure ()
      Node x xs : rest -> do
        r <- lift (test x)
        Pipes.yield r
        when (is failure r) $
          go xs
        go rest

drawShrinkForest :: Show a => Forest (Shrink a) -> String
drawShrinkForest = drawForest . map (map show)