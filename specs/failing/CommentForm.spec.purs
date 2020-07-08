module CommentFormSpecification where

import WebCheck
import Data.Array (head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), length, split, trim)
import Data.Tuple (Tuple(..))

readyWhen :: String
readyWhen = "form"

actions :: Actions
actions = clicks <> foci <> [ Tuple 1 (KeyPress ' '), Tuple 1 (KeyPress 'a') ]

proposition :: Boolean
proposition =
  let
    commentPosted = isVisible ".comment-display" && commentIsValid && not (isVisible "form")

    invalidComment = not (isVisible ".comment-display") && isVisible "form"

    postComment = isVisible "form" && next (commentPosted || invalidComment)
  in
    isVisible "form" && always postComment

commentIsValid :: Boolean
commentIsValid = commentLength (fromMaybe "" (_.textContent <$> queryOne ".comment" { textContent })) >= 3
  where
  commentLength t = length (trim (fromMaybe "" (head =<< tail (split (Pattern ": ") t))))

isVisible :: Selector -> Boolean
isVisible sel = Just "block" == (_.display <$> queryOne sel { display: cssValue "display" })
