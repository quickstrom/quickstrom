module WTP.PureScriptTest where
  
import Prelude

foo :: Int
foo =
    let x = 1
        y = x + 2
        z = x + y
    in z

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
