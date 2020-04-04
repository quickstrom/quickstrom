{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module WTP.Verify where

import Data.Maybe (fromMaybe)
import Prelude hiding (Bool (..), not)
import Control.Monad.Except (MonadError (..))
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import Data.Text (Text)
import WTP.Core

type Elements = HashMap Selector [Element]

data Step = Step {queriedElements :: Elements }
  deriving (Eq, Show)

type Result = Either Failure

data Failure
  = Undetermined
  | Rejected Text
  deriving (Eq, Show)

invert :: Result () -> Result ()
invert = \case
  Left Undetermined -> throwError Undetermined
  Left Rejected{} -> pure ()
  Right () -> throwError (Rejected "false")

runQuery :: Query a -> Elements -> Result a 
runQuery query elements =
  case query of
    Query selector -> case HashMap.lookup selector elements of
      Just (a : _) -> pure (Just a)
      _ -> pure Nothing
    QueryAll selector -> pure (fromMaybe [] (HashMap.lookup selector elements))
    Require query' ->
      runQuery query' elements
        >>= maybe (throwError (Rejected (Text.pack (show query) <> " returned no result"))) pure
    Get attr query' -> do
      _element <- runQuery query' elements
      case attr of
        InnerHTML -> pure mempty
        InnerText -> pure mempty
        ClassList -> pure []
    Map f query' -> f <$> runQuery query' elements

runAssertion :: Assertion a -> a -> Result ()
runAssertion assertion a =
  case assertion of
    Equals expected ->
      if a == expected
      then pure ()
      else throwError (Rejected (Text.pack (show a) <> " does not equal " <> Text.pack (show expected)))
    Contains needle ->
      if needle `Text.isInfixOf` a
      then pure ()
      else throwError (Rejected (Text.pack (show a) <> " does not contain " <> Text.pack (show needle)))
    Satisfies predicate ->
      if predicate a
      then pure ()
      else throwError (Rejected (Text.pack (show a) <> " does not satisfy custom predicate"))

verify :: Formula -> [Step] -> Result ()
verify = go
  where
    go spec steps =
      case steps of
        [] -> throwError Undetermined
        current : rest ->
          case spec of
            True -> pure ()
            Not p -> go p steps
            p `Or` q -> go p steps <> go q steps
            p `Until` q -> case go p [current] of
              Left Undetermined -> throwError Undetermined
              Left Rejected{} -> go q rest
              Right{} -> go (p `Until` q) rest
            Assert query assertion ->
              runAssertion assertion =<< runQuery query (queriedElements current)
