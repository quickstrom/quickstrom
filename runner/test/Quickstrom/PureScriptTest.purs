module Quickstrom.PureScriptTest where

import Quickstrom
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (evalState, get, modify)
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Random.LCG (lcgNext, lcgPerturb, mkSeed, unSeed)

mutuallyRecTop :: Int
mutuallyRecTop = mutuallyRec1 10

mutuallyRec1 :: Int -> Int
mutuallyRec1 n
  | n > 0 = mutuallyRec2 (n - 1)
  | otherwise = n

mutuallyRec2 :: Int -> Int
mutuallyRec2 n
  | n > 0 = mutuallyRec1 (n - 2)
  | otherwise = n

mutuallyRecLet :: Int
mutuallyRecLet =
  let
    mutuallyRecLet1 :: Int -> Int
    mutuallyRecLet1 n
      | n > 0 = mutuallyRecLet2 (n - 1)
      | otherwise = n

    mutuallyRecLet2 :: Int -> Int
    mutuallyRecLet2 n
      | n > 0 = mutuallyRecLet1 (n - 2)
      | otherwise = n
  in
    mutuallyRecLet1 10

unfoldrNumbers :: Array Int
unfoldrNumbers =
  unfoldr
    (\n -> if n > 0 then Just (Tuple n (n - 1)) else Nothing)
    10

convertNum :: Number
convertNum = toNumber 1

testState :: Int
testState = evalState f 10
  where
  f = do
    _ <- modify (_ - 1)
    n <- get
    if n > 0 then f else pure n

testReader :: Int
testReader = runReader f 10
  where
  f :: Reader Int Int
  f = do
    n <- ask
    if n > 0 then local (_ - 1) f else pure n

testIdentityBind :: Int
testIdentityBind = un Identity (go 10)
  where
  go n = do
    if n > 0 then go (n - 1) else pure n

seeds :: Array Int
seeds = go (mkSeed 1000) 20
  where
  go seed n
    | n > 0 = [ unSeed (lcgPerturb 1000.0 seed) ] <> go (lcgNext seed) (n - 1)
    | otherwise = []

partial :: Int
partial = unsafePartial fromJust (pure 123)

paragraph :: Maybe { text :: String }
paragraph = queryOne "p" { text: textContent }

lazyNext :: String
lazyNext = let f a = next a in f (maybe "" _.text paragraph)

lazyNextNext :: String
lazyNextNext = let f a = next a in f (maybe "" _.text (next paragraph))

lazyInArray :: String
lazyInArray =
  let
    xs :: Array (Maybe String)
    xs = [ _.text <$> paragraph ]

    x :: Maybe String
    x = next (join (xs Array.!! 0))
  in
    fromMaybe "" x

lazyUnchanged :: Array Boolean
lazyUnchanged = [ unchanged paragraph, unchanged (next paragraph), unchanged [ next paragraph ] ]

testOneQuery :: String
testOneQuery = maybe "" _.text paragraph

testNextOneQuery :: String
testNextOneQuery = next testOneQuery

partialUntil :: Boolean
partialUntil = let f = until false in f true

passNext :: Boolean
passNext = let f = (_ $ true) in f next

tla1 = next false || true

tla2 = true || next false

tla3 = next true

tla4 = true && always true

tla5 = always (next true)

tla6 = always (next false || next true)

tla7 = until false true

tla8 = until false false

tla9 = until true false

tla10 = until true (testOneQuery == "baz")
