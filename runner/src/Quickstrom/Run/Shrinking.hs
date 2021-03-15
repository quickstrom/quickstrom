{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Shrinking where

import Control.Lens
import Control.Lens.Extras (is)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.String (String)
import Data.Tree
import qualified Pipes
import Quickstrom.Prelude hiding (Prefix)

newtype Prefix a = Prefix a
  deriving (Eq, Show, Hashable)

shrinkActions :: [a] -> Forest (Prefix [a])
shrinkActions = shrinkForest shrinkList

shrinkForest :: (Ord strategy, Enum strategy, Bounded strategy) => (strategy -> a -> [a]) -> a -> Forest (Prefix a)
shrinkForest shrink = go [minBound .. maxBound]
  where
    go [] _ = []
    go strategies x =
      [ Node (Prefix x') (go (filter (>= strategy) strategies) x')
        | strategy <- strategies,
          x' <- shrink strategy x
      ]

pruneDuplicates :: (Hashable a, Eq a) => Forest a -> Forest a
pruneDuplicates f = evalState (pruneForest f) mempty
  where
    pruneForest :: (Hashable a, Eq a) => Forest a -> State (HashSet a) (Forest a)
    pruneForest = map fold . traverse pruneNode
    pruneNode :: (Hashable a, Eq a) => Tree a -> State (HashSet a) (Forest a)
    pruneNode (Node x xs) = do
      seen <- get
      if HashSet.member x seen
        then pruneForest xs
        else do
          put (HashSet.insert x seen)
          pure . Node x <$> pruneForest xs

data ListStrategy = Half | Quarter | Single
  deriving (Eq, Show, Ord, Enum, Bounded)

shrinkList :: ListStrategy -> [a] -> [[a]]
shrinkList _ [] = []
shrinkList strategy as =
  filter (not . null) $
    let q :: Double
        q = fromIntegral (length as) / 4
     in case strategy of
          Half -> [take (floor (q * 2)) as]
          Quarter -> [take (floor (q * 3)) as]
          Single -> drop (floor (q * 3) + 1) (initSafe (inits as))

traverseShrinks :: Monad m => (Prefix a -> m b) -> Prism' b x -> Forest (Prefix a) -> Pipes.Producer b m ()
traverseShrinks test failure = go
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