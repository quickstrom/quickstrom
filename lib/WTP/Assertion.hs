{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module WTP.Assertion where

import Data.Text (Text)
import Control.Monad.Freer (Eff, Members)
import WTP.Query (Query)
import WTP.Result (Result(..))
import qualified Data.Text as Text

data Assertion a where
  Equals :: (Show a, Eq a) => a -> Assertion a
  Contains :: Text -> Assertion Text
  Satisfies :: Show a => (a -> Bool) -> Assertion a

instance Show (Assertion a) where
  show = \case
    Equals expected -> "(Equals " <> show expected <> ")"
    Contains t -> "(Contains " <> show t <> ")"
    Satisfies _ -> "(Satisfies _)"

type IsQuery eff = Members '[Query] eff

data QueryAssertion where
  QueryAssertion :: Show a => Eff '[Query] a -> Assertion a -> QueryAssertion

instance Show QueryAssertion where
  show (QueryAssertion _q a) = "(QueryAssertion _ " <> show a <> ")"

runAssertion :: Assertion a -> a -> Result
runAssertion assertion a =
  case assertion of
    Equals expected ->
      if a == expected
        then Accepted
        else Rejected
    Contains needle ->
      if needle `Text.isInfixOf` a
        then Accepted
        else Rejected
    Satisfies predicate ->
      if predicate a
        then Accepted
        else Rejected