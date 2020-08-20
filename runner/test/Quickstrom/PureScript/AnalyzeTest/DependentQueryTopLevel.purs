module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length)

readyWhen = "body"

actions = []

q1 :: String
q1 = fromMaybe "fallback" (map _.textContent (queryOne ".selector" { textContent }))

q2 :: Int
q2 = length (queryAll q1 { textContent })

proposition :: Boolean
proposition = q2 == 1
