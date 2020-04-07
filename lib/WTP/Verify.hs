{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WTP.Verify where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Formula
import WTP.Query
import Prelude hiding (Bool (..), not)
import Debug.Trace (traceShow)

type Elements = HashMap Selector [Element]

data ElementStateValue where
  ElementStateValue :: (Typeable a) => ElementState a -> a -> ElementStateValue

findElementState :: Typeable a => ElementState a -> [ElementStateValue] -> Maybe a
findElementState _ [] = Nothing
findElementState (state :: ElementState s1) (ElementStateValue (_ :: ElementState s2) value : rest) =
  case eqTypeRep (typeRep @s1) (typeRep @s2) of
    Just HRefl -> Just value
    Nothing -> findElementState state rest

type States = HashMap Element [ElementStateValue]

data Step = Step {queriedElements :: Elements, elementStates :: States}

type Result = Either Failure

data Failure
  = Undetermined
  | Rejected Text
  | MissingState Text
  deriving (Eq, Show)

invert :: Result () -> Result ()
invert = \case
  Left Rejected {} -> pure ()
  Left e -> Left e
  Right () -> Left (Rejected "false")

invert' :: Member (Error Failure) effs => Eff effs () -> Eff effs ()
invert' ma = do
  e <- lowerEither @Failure ma
  liftEither (invert e)

mapError :: (e -> f) -> Eff (Error e ': effs) a -> Eff (Error f ': effs) a
mapError f = reinterpret (\(Error e) -> throwError (f e))

lowerEither :: forall e effs a. Member (Error e) effs => Eff effs a -> Eff effs (Either e a)
lowerEither ma = catchError (Right <$> ma) (pure . Left)

liftEither :: Member (Error e) effs => Either e a -> Eff effs a
liftEither = either throwError pure

runQueryPure :: Member (Error Text) effs => Elements -> States -> Eff (Query ': effs) a -> Eff effs a
runQueryPure elements statesByElement =
  interpret
    ( \case
        Query selector -> case HashMap.lookup selector elements of
          Just (a : _) -> pure (Just a)
          _ -> pure Nothing
        QueryAll selector -> pure (fromMaybe [] (HashMap.lookup selector elements))
        Get state element ->
          let states = fromMaybe mempty (HashMap.lookup element statesByElement)
              msg = "Could not find state (" <> Text.pack (show state) <> ") for element (" <> Text.pack (show element)
           in maybe (throwError msg) pure (findElementState state states)
    )

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

verify :: Formula (Query ': Error Text ': effs) -> [Step] -> Eff (Error Failure ': effs) ()
verify spec steps = case steps of
  [] -> traceShow spec $ throwError Undetermined
  current : rest ->
    case spec of
      True -> pure ()
      Not p -> invert' (verify p steps)
      p `Or` q -> do
        r1 <- lowerEither @Failure (verify p steps)
        r2 <- lowerEither @Failure (verify q steps)
        liftEither (r1 <> r2)
      p `Until` q ->
        catchError
          (verify p [current] >> verify (p `Until` q) rest)
          ( \case
              Rejected {} -> verify q rest
              e -> throwError e
          )
      Assert query' assertion ->
        runAssertion assertion
          =<< mapError Rejected (runQueryPure (queriedElements current) (elementStates current) query')
