module TodoMVC where

import Quickstrom
import Quickstrom.Spec (Action(..), BaseAction(..))
import Data.Array (filter, foldMap, head, last, zip)
import Data.Foldable (length)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))

queries :: Queries
queries = classBasedQueries

readyWhen :: Selector
readyWhen = queries.newTodo

actions :: Actions
actions = appFoci <> appClicks <> appKeyPresses
  where
  appClicks =
    [ Tuple 5 $ Single $ Click queries.filters.notSelected
    , Tuple 1 $ Single $ Click queries.filters.selected
    , Tuple 1 $ Single $ Click queries.toggleAll
    , Tuple 1 $ Single $ Click queries.destroy
    ]

  appFoci = [ Tuple 5 $ Single (Focus queries.newTodo) ]

  appKeyPresses =
    [ Tuple 5 $ Single (keyPress 'a')
    , Tuple 5 $ Single (specialKeyPress KeyReturn)
    ]

proposition :: Boolean
proposition =
  initial
    && always
        ( enterText
            || addNew
            || changeFilter
            || checkOne
            || uncheckOne
            || delete
            || toggleAll
        )
    && always hasFilters
  where
  initial :: Boolean
  initial =
    (selectedFilter == Nothing || selectedFilter == Just All)
      && (numItems == 0)
      && (pendingText == "")

  enterText :: Boolean
  enterText =
    pendingText /= next pendingText
      && itemTexts
      == next itemTexts
      && selectedFilter
      == next selectedFilter

  changeFilter :: Boolean
  changeFilter = case selectedFilter, next selectedFilter of
    Nothing, Just All -> true -- adding the first todo item switches to the All filter
    Nothing, _ -> false -- any other transition from an empty todo list is incorrect
    _, Nothing -> false -- removing the last todo item is not handled by this action
    Just All, _ -> numItems >= next numItems
    _, Just Active -> next (numItemsLeft == Just numUnchecked && numItems == numUnchecked)
    Just f1, Just f2
      | f1 == f2 -> false
      | otherwise -> pendingText == next pendingText

  addNew =
    next (pendingText == "")
      && case next selectedFilter of
          Just All -> Just pendingText == next lastItemText
          Just Active -> Just pendingText == next lastItemText
          Just Completed -> itemTexts == next itemTexts
          Nothing -> false

  checkOne =
    pendingText == next pendingText
      && selectedFilter
      == next selectedFilter
      && (selectedFilter /= Just Completed)
      && ( (selectedFilter == Just All)
            `implies`
              (numItems == next numItems && numChecked < next numChecked)
        )
      && ( (selectedFilter == Just Active)
            `implies`
              (numItems > next numItems && numItemsLeft > next numItemsLeft)
        )

  uncheckOne =
    pendingText == next pendingText
      && selectedFilter
      == next selectedFilter
      && (selectedFilter /= Just Active)
      && ( (selectedFilter == Just All)
            `implies`
              (numItems == next numItems && numChecked > next numChecked)
        )
      && ( (selectedFilter == Just Completed)
            `implies`
              (numItems > next numItems && numItemsLeft < next numItemsLeft)
        )

  delete =
    pendingText == next pendingText
      && case selectedFilter, numItems of
          _, 1 -> next (numItems == 0)
          Just filter, n ->
            selectedFilter == next selectedFilter
              && next numItems
              == n
              - 1
              && case filter of
                  All -> true
                  Active -> numItemsLeft == next ((_ - 1) <$> numItemsLeft)
                  Completed -> numItemsLeft == next numItemsLeft
          Nothing, _ -> false

  toggleAll =
    pendingText == next pendingText
      && selectedFilter
      == next selectedFilter
      && case selectedFilter of
          Just All -> numItems == next numItems && next (numItems == numChecked)
          Just Active ->
            (numItems > 0) `implies` (next numItems == 0)
              || (numItems == 0) `implies` (next numItems > 0)
          Just Completed -> numItems + fromMaybe 0 numItemsLeft == (next numItems)
          Nothing -> false

  selectedFilter = do
    f <- queryOne queries.filters.selected { text: textContent }
    parse f.text
    where
    parse = case _ of
      "All" -> pure All
      "Active" -> pure Active
      "Completed" -> pure Completed
      _ -> Nothing

  hasFilters = case length items, availableFilters of
    0, _ -> true
    _, [ "All", "Active", "Completed" ] -> true
    _, _ -> false
    where
    availableFilters = map _.textContent (queryAll queries.filters.all { textContent })

  items :: Array { text :: String }
  items =
    foldMap (\(Tuple li label) -> if li.display /= "none" then pure label else mempty)
      ( zip
          (queryAll queries.items.listItems { display: cssValue "display" })
          (queryAll queries.items.labels { text: textContent })
      )

  itemTexts = map _.text items

  lastItemText = last itemTexts

  numItems :: Int
  numItems = length items

  checkboxes = queryAll queries.items.checkboxes { checked: checked }

  numUnchecked :: Int
  numUnchecked = length (filter (not <<< _.checked) checkboxes)

  numChecked :: Int
  numChecked = length (filter _.checked checkboxes)

  pendingText :: String
  pendingText = case queryOne queries.newTodo { value: value } of
    Just el -> el.value
    Nothing -> ""

  numItemsLeft :: Maybe Int
  numItemsLeft = do
    strong <- queryOne queries.todoCount { text: textContent }
    first <- head (split (Pattern " ") (trim strong.text))
    Int.fromString first

data Filter
  = All
  | Active
  | Completed

derive instance eqFilter :: Eq Filter

type Queries
  = { top :: Selector
    , filters ::
      { all :: Selector
      , selected :: Selector
      , notSelected :: Selector
      }
    , toggleAll :: Selector
    , destroy :: Selector
    , newTodo :: Selector
    , todoCount :: Selector
    , items ::
      { listItems :: Selector
      , labels :: Selector
      , checkboxes :: Selector
      }
    }

classBasedQueries :: Queries
classBasedQueries =
  { top: ".todoapp"
  , filters:
    { all: ".todoapp .filters a"
    , selected: ".todoapp .filters a.selected"
    , notSelected: ".todoapp .filters a:not(.selected)"
    }
  , destroy: ".todoapp .destroy"
  , toggleAll: ".todoapp label[for=toggle-all]"
  , newTodo: ".todoapp .new-todo"
  , todoCount: ".todoapp .todo-count strong"
  , items:
    { listItems: ".todo-list li"
    , labels: ".todo-list li label"
    , checkboxes: ".todo-list li input[type=checkbox]"
    }
  }

idBasedQueries :: Queries
idBasedQueries =
  { top: "#todoapp"
  , filters:
    { all: "#todoapp #filters a"
    , selected: "#todoapp #filters a.selected"
    , notSelected: "#todoapp #filters a:not(.selected)"
    }
  , destroy: "#todoapp .destroy"
  , toggleAll: "#todoapp label[for=toggle-all]"
  , newTodo: "#todoapp #new-todo"
  , todoCount: "#todoapp #todo-count strong"
  , items:
    { listItems: "#todo-list li"
    , labels: "#todo-list li label"
    , checkboxes: "#todo-list li input[type=checkbox]"
    }
  }
