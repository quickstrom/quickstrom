module WebCheck.PureScript.TodoMVC where

import WebCheck.DSL

import Data.Array (elem, filter, head, last)
import Data.Foldable (length)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))

name :: String
name = "react"

origin :: Path
origin = "http://todomvc.com/examples/" <> name <> "/"

readyWhen :: Selector
readyWhen = ".todoapp"

actions :: Actions
actions = appFoci <> appClicks <> appKeyPresses 
  where
    appClicks = 
      [ Tuple 5 (Click ".todoapp .filters a:not(.selected)")
      , Tuple 1 (Click ".todoapp .filters a.selected")
      , Tuple 1 (Click ".todoapp label[for=toggle-all]")
      , Tuple 1 (Click ".todoapp .destroy")
      ]
    appFoci = [ Tuple 5 (Focus ".todoapp .new-todo") ]
    appKeyPresses =
      [ Tuple 5 (keyPress 'a')
      , Tuple 5 (specialKeyPress KeyEnter)
      ]

proposition :: Boolean
proposition =
  initial
  && always (enterText
             || addNew
             || changeFilter
             || checkOne
             || uncheckOne
             || delete
             || toggleAll
            )
  where

    initial :: Boolean
    initial =
      (selectedFilter == Nothing || selectedFilter == Just All)
        && (numItems == 0)
        && (pendingText == "")
    
    enterText :: Boolean
    enterText =
      pendingText /= next pendingText
        && itemTexts == next itemTexts
        && selectedFilter == next selectedFilter
    
    changeFilter :: Boolean
    changeFilter =
      case selectedFilter, next selectedFilter of
          Nothing, Just All -> true -- adding the first todo item switches to the All filter
          Nothing, _ -> false -- any other transition from an empty todo list is incorrect
          _, Nothing -> false -- removing the last todo item is not handled by this action
          Just All, _ -> numItems >= next numItems
          _, Just Active -> next (numItemsLeft == Just numUnchecked && numItems == numUnchecked)
          Just f1, Just f2
            | f1 == f2 -> false
            | otherwise ->
                -- NOTE: AngularJS && Mithril implementations are
                -- inconsistent with the other JS implementations, in that
                -- they clear the input field when the filter is changed.
                (pendingText == next pendingText)
    
    addNew =
      Just pendingText == next lastItemText
        && next (pendingText == "")

    checkOne =
      pendingText == next pendingText
        && selectedFilter == next selectedFilter
        && (selectedFilter /= Just Completed)
        && ( (selectedFilter == Just All)
                `implies` (numItems == next numItems && numChecked < next numChecked)
            )
        && ( (selectedFilter == Just Active)
                `implies` (numItems > next numItems && numItemsLeft > next numItemsLeft)
            )

    uncheckOne =
      pendingText == next pendingText
        && selectedFilter == next selectedFilter
        && (selectedFilter /= Just Active)
        && ( (selectedFilter == Just All)
                `implies` (numItems == next numItems && numChecked > next numChecked)
            )
        && ( (selectedFilter == Just Completed)
                `implies` (numItems > next numItems && numItemsLeft < next numItemsLeft)
            )
    
    delete =
      pendingText == next pendingText
        && case selectedFilter, numItems of
          _, 1 -> next selectedFilter == Nothing
          Just filter, n -> 
            selectedFilter == next selectedFilter
            && next numItems == n - 1 
            && case filter of
                 All -> true
                 Active -> numItemsLeft == next ((_ - 1) <$> numItemsLeft)
                 Completed -> numItemsLeft == next numItemsLeft
          Nothing, _ -> false

    toggleAll =
      pendingText == next pendingText
        && selectedFilter == next selectedFilter
        && case selectedFilter of
          Just All -> numItems == next numItems && next (numItems == numChecked)
          Just Active -> (numItems > 0) `implies` (next numItems == 0)
                        || (numItems == 0) `implies` (next numItems > 0)
          Just Completed -> numItems + fromMaybe 0 numItemsLeft == (next numItems)
          Nothing -> false
    
    
    selectedFilter = do
      f <- queryOne ".todoapp .filters .selected" { text: textContent }
      parse f.text
      where
        parse = case _ of
          "All" -> pure All
          "Active" -> pure Active
          "Completed" -> pure Completed
          _ -> Nothing
    
    items :: Array { text :: String }
    items = queryAll ".todo-list li label" { text: textContent }
    
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

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter
