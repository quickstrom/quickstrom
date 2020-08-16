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
