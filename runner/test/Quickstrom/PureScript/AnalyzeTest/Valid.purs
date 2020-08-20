module Spec where

import Quickstrom
import Data.Maybe (Maybe(..), isJust)

readyWhen = "body"

actions = []

proposition :: Boolean
proposition =
  let
    localQuery = isJust (queryOne "p" { display: cssValue "display" })
  in
    localQuery && always (localQuery || transientQuery || inRecordQuery.test)

transientQuery :: Boolean
transientQuery = isJust (queryOne "button" { textContent })

inRecordQuery :: { test :: Boolean }
inRecordQuery = { test: isJust (queryOne "a" { textContent }) }

unused :: Boolean
unused = isJust (queryOne "x" { textContent })
