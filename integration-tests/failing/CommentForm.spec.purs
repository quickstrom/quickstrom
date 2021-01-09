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
  [ Tuple 3 (Single $ Focus "input[type=text]:nth-child(1)")
  -- This spec is flaky. It only finds the bug on some runs, so the following action
  -- is commented out to increase chances of a failed example:
  --
  --   , Tuple 1 (Single $ Focus "input[type=text]:nth-child(2)")
  , Tuple 5 (Single $ Click "input[type=submit]")
  , Tuple 5 (Single $ KeyPress ' ')
  , Tuple 5 (Single $ KeyPress 'a')
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
