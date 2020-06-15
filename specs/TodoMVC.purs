module WebCheck.PureScript.TodoMVC where

import WebCheck.DSL

import Data.Array (filter, head, last)
import Data.Foldable (length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)

origin :: Path
origin = "http://todomvc.com/examples/angularjs/"

readyWhen :: Selector
readyWhen = ".todoapp"

actions :: Array Action
actions = appFoci <> appClicks <> pure (keyPress 'a')
  where
    appClicks = [ Click ".todoapp button", Click ".todoapp input[type=button]", Click ".todoapp a" ]
    appFoci = [ Focus ".todoapp input", Focus ".todoapp textarea" ]

proposition :: Boolean
proposition =
  initial
  && always (enterText
             || addNew
             || changeFilter
             || checkOne
             || uncheckOne
             || toggleAll
            )
  where

    initial :: Boolean
    initial =
      (currentFilter == Nothing || currentFilter == Just "All")
        && (numItems == 0)
        && (pendingText == "")
    
    enterText :: Boolean
    enterText =
      pendingText /= next pendingText
        && itemTexts == next itemTexts
        && currentFilter == next currentFilter
    
    changeFilter :: Boolean
    changeFilter =
      (currentFilter /= next currentFilter)
        && (currentFilter == Just "All") `implies` (numItems >= next numItems)
        && ( next
                ( (currentFilter == Just "Active")
                  `implies` (numItemsLeft == (Just numUnchecked) && numItems == numUnchecked)
                )
            )
        -- NOTE: AngularJS && Mithril implementations are
        -- inconsistent with the other JS implementations, in that
        -- they clear the input field when the filter is changed.

        -- && not (name `Array.elem` ["angularjs", "mithril"]) ==> pendingText == next pendingText
        && pendingText == next pendingText
    
    addNew =
      Just pendingText == next lastItemText
        && next (pendingText == "")

    checkOne =
      pendingText == next pendingText
        && currentFilter == next currentFilter
        && (currentFilter /= Just "Completed")
        && ( (currentFilter == Just "All")
                `implies` (numItems == next numItems && numChecked < next numChecked)
            )
        && ( (currentFilter == Just "Active")
                `implies` (numItems > next numItems && numItemsLeft > next numItemsLeft)
            )

    uncheckOne =
      pendingText == next pendingText
        && currentFilter == next currentFilter
        && (currentFilter /= Just "Active")
        && ( (currentFilter == Just "All")
                `implies` (numItems == next numItems && numChecked > next numChecked)
            )
        && ( (currentFilter == Just "Completed")
                `implies` (numItems > next numItems && numItemsLeft < next numItemsLeft)
            )
    
    toggleAll =
      Just pendingText == next lastItemText
        && currentFilter == next currentFilter
        && ( (currentFilter == Just "All")
                `implies` (numItems == next numItems && next (numItems == numChecked))
            )
        && ( (currentFilter == Just "Active")
                `implies` ( (numItems > 0) `implies` (next numItems == 0)
                        || (numItems == 0) `implies` (next numItems > 0)
                    )
            )
    
    
    currentFilter = do
      f <- queryOne ".todoapp .filters .selected" { text: textContent }
      pure f.text
    
    items :: Array { text :: String }
    items = queryAll ".todo-list li" { text: textContent }
    
    itemTexts = map _.text items
    
    lastItemText = last itemTexts
    
    numItems :: Int
    numItems = length items
    
    checkboxes = queryAll ".todo-list li input[type=checkbox]" { checked: checked }
    
    numUnchecked :: Int
    numUnchecked = length (filter (not <<< _.checked) checkboxes)
    
    numChecked :: Int
    numChecked = length (filter _.checked checkboxes)
    
    pendingText :: String
    pendingText = case queryOne ".new-todo" { value: value } of
      Just el -> el.value
      Nothing -> ""
    
    numItemsLeft :: Maybe Int
    numItemsLeft = do
      strong <- queryOne ".todoapp .todo-count strong" { text: textContent }
      first <- head (split (Pattern " ") strong.text)
      Int.fromString first
