{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module WTP.Verify where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Prelude                   hiding (Bool (..), not)
import           WTP.Core

type Elements = HashMap Selector [Element]

data Step = Step {queriedElements :: Elements}
  deriving (Eq, Show)

type Result = Either Failure

data Failure
  = Undetermined
  | Rejected Text
  deriving (Eq, Show)

invert :: Result () -> Result ()
invert = \case
  Left Undetermined -> Left Undetermined
  Left Rejected {} -> pure ()
  Right () -> Left (Rejected "false")

newtype QueryC m a = QueryC { runQueryC :: m a }
  deriving (Applicative, Functor, Monad)

runQueryPure :: Eff '[Query] a -> Elements -> Eff '[Error Failure] a
runQueryPure query elements =
  reinterpret
    ( \case
        Query selector -> case HashMap.lookup selector elements of
          Just (a : _) -> pure (Just a)
          _            -> pure Nothing
        QueryAll selector -> pure (fromMaybe [] (HashMap.lookup selector elements))
        Get attr _ -> case attr of
          InnerHTML -> pure mempty
          InnerText -> pure mempty
          ClassList -> pure []
        Require ma -> maybe (throwError (Rejected "Required value is Nothing")) pure ma
    )
    query

runAssertion :: Member (Error Failure) effs => Assertion a -> a -> Eff effs ()
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

verify :: Formula -> [Step] -> Eff '[Error Failure] ()
verify spec steps = case steps of
  [] -> throwError Undetermined
  current : rest ->
    case spec of
      True -> pure ()
      Not p -> verify p steps
      p `Or` q -> do
        (r1 :: Either Failure ()) <- catchError (Right <$> verify p steps) (pure . Left)
        (r2 :: Either Failure ()) <- catchError (Right <$> verify q steps) (pure . Left)
        either throwError pure (r1 <> r2)
      p `Until` q ->
        catchError
          (verify p [current] >> verify (p `Until` q) rest)
          ( \case
              Undetermined -> throwError Undetermined
              Rejected {} -> verify q rest
          )
      Assert query' assertion ->
        runAssertion assertion =<< runQueryPure query' (queriedElements current)
