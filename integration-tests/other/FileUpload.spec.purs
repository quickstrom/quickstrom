module Specification where

import Quickstrom
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

readyWhen = "button"

actions = clicks <> [ Tuple 10 (Single $ Click "input[type=file]") ]

proposition :: Boolean
proposition =
  let
    initial = messageIs ""

    selectFile = selectedFile == Just "" && next (selectedFile /= Just "")

    upload = selectedFile /= Just "" && next (messageIs "You uploaded:")
  in
    initial && always (selectFile || upload)

selectedFile :: Maybe String
selectedFile = _.value <$> queryOne "#file" { value }

messageIs :: String -> Boolean
messageIs message = queryOne ".message" { textContent } == Just { textContent: message }
