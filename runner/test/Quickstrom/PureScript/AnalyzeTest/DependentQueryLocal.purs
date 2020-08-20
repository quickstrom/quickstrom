module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length)

readyWhen = "body"

actions = []

q :: Int
q =
  let
    q1 = fromMaybe "fallback" (map _.textContent (queryOne ".selector" { textContent }))
  in
    length (queryAll q1 { textContent })

proposition :: Boolean
proposition = q == 1
