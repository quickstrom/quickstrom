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
import qualified Data.Bool as Bool
import Debug.Trace (traceM, traceShowM)

data ElementStateValue where
  ElementStateValue :: (Typeable a, Show a, Eq a) => ElementState a -> a -> ElementStateValue

deriving instance Show ElementStateValue

instance Eq ElementStateValue where
  ElementStateValue (s1 :: ElementState t1) (v1 :: t1) == ElementStateValue (s2 :: ElementState t2) (v2 :: t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> s1 == s2 && v1 == v2
      Nothing -> Bool.False


findElementState :: Typeable a => ElementState a -> [ElementStateValue] -> Maybe a
findElementState _ [] = Nothing
findElementState (state :: ElementState s1) (ElementStateValue (_ :: ElementState s2) value : rest) =
  case eqTypeRep (typeRep @s1) (typeRep @s2) of
    Just HRefl -> Just value
    Nothing -> findElementState state rest

type Elements = HashMap Selector [Element]

type States = HashMap Element [ElementStateValue]

data Step = Step {queriedElements :: Elements, elementStates :: States} deriving (Show)

data Result
  = Accepted
  | Rejected Text
  | Undetermined
  deriving (Eq, Show)

invert :: Result -> Result
invert = \case
  Accepted -> Rejected "false"
  Rejected {} -> Accepted
  r -> r

instance Semigroup Result where
  Accepted <> _ = Accepted
  Undetermined <> _ = Undetermined
  _ <> r = r

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

runAssertion :: Assertion a -> a -> Result
runAssertion assertion a =
  case assertion of
    Equals expected ->
      if a == expected
        then Accepted
        else Rejected (Text.pack (show a) <> " does not equal " <> Text.pack (show expected))
    Contains needle ->
      if needle `Text.isInfixOf` a
        then Accepted
        else Rejected (Text.pack (show a) <> " does not contain " <> Text.pack (show needle))
    Satisfies predicate ->
      if predicate a
        then Accepted
        else Rejected (Text.pack (show a) <> " does not satisfy custom predicate")

verify :: Formula (Query ': Error Text ': effs) -> [Step] -> Eff effs Result
verify spec steps = case steps of
  [] -> pure Undetermined
  current : rest ->
    case spec of
      True -> pure Accepted
      Not p -> invert <$> verify p steps
      p `Or` q -> do
        r1 <- verify p steps
        r2 <- verify q steps
        pure (r1 <> r2)
      p `Until` q -> do
        r1 <- verify p [current]
        r2 <- verify q rest
        case (r1, r2) of
          (Accepted, Rejected{}) -> verify (p `Until` q) rest 
          (Accepted, _) -> pure r2
          (_, Accepted) -> pure Accepted
          (r1, _) -> pure r1
      Assert query' assertion ->
        either Rejected (runAssertion assertion)
          <$> runError (runQueryPure (queriedElements current) (elementStates current) query')
