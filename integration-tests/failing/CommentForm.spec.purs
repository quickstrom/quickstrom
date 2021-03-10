module CommentFormSpecification where

import Quickstrom
import Data.Array (head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), length, split, trim)
import Data.Tuple (Tuple(..))

readyWhen :: String
readyWhen = "form"

actions :: Actions
actions =
  [ Focus "input[type=text]:nth-child(1)" `weighted` 3
  -- This spec is flaky. It only finds the bug on some runs, so the following action
  -- is commented out to increase chances of a failed example:
  --
  --   , Tuple 1 (Single $ Focus "input[type=text]:nth-child(2)")
  , Click "input[type=submit]" `weighted` 5
  , KeyPress ' ' `weighted` 5
  , KeyPress 'a' `weighted` 5
  ]

proposition :: Boolean
proposition =
  let
    commentPosted = commentIsVisible && commentIsValid && not formIsVisible

    invalidComment = not commentIsVisible && formIsVisible

    postComment = formIsVisible && next (commentPosted || invalidComment)
  in
    formIsVisible && always postComment

commentIsValid :: Boolean
commentIsValid = commentLength (fromMaybe "" (_.textContent <$> queryOne ".comment" { textContent })) >= 3
  where
  commentLength t = length (trim (fromMaybe "" (head =<< tail (split (Pattern ": ") t))))

commentIsVisible :: Boolean
commentIsVisible = Just "block" == (_.display <$> queryOne ".comment-display" { display: cssValue "display" })

formIsVisible :: Boolean
formIsVisible = Just "block" == (_.display <$> queryOne "form" { display: cssValue "display" })
