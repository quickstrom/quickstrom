module WTP.PureScriptTest where
  
import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (evalState, get, modify, runState, state)
import Data.Array.NonEmpty as Array
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Random.LCG (lcgNext, mkSeed, unSeed)

mutuallyRecTop :: Int
mutuallyRecTop = mutuallyRec1 10

mutuallyRec1 :: Int -> Int
mutuallyRec1 n 
    | n > 0 = mutuallyRec2 (n - 1)
    | otherwise = n

mutuallyRec2 :: Int -> Int
mutuallyRec2 n 
    | n > 0= mutuallyRec1 (n - 2)
    | otherwise = n

mutuallyRecLet :: Int
mutuallyRecLet = 
    let mutuallyRecLet1 :: Int -> Int
        mutuallyRecLet1 n 
            | n > 0 = mutuallyRecLet2 (n - 1)
            | otherwise = n

        mutuallyRecLet2 :: Int -> Int
        mutuallyRecLet2 n 
            | n > 0= mutuallyRecLet1 (n - 2)
            | otherwise = n
    in mutuallyRecLet1 10

unfoldrNumbers :: Array Int
unfoldrNumbers = 
    unfoldr 
    (\n -> if n > 0 then Just (Tuple n (n - 1)) else Nothing)
    10

convertNum :: Number
convertNum =  toNumber 1

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