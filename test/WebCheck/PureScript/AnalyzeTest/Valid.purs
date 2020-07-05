module ValidSpec where

import WebCheck
import Data.Maybe (Maybe(..))

readyWhen = "body"

actions = []

proposition :: Boolean
proposition =
  let localQuery = queryOne "p" { display: cssValue "display" } == Just { display: "none" }
  in localQuery && always (localQuery || transientQuery)

transientQuery :: Boolean
transientQuery = queryOne "button" { textContent } == Just { textContent: "foo" }

