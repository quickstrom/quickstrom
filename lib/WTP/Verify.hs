{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WTP.Verify
  ( assertQuery,
    Step (..),
    ElementStateValue (..),
  )
where

import Control.Monad.Freer
import qualified Data.Bool as Bool
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Assertion
import WTP.Query
import WTP.Result
import Prelude hiding (Bool (..), not)

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

runQuery :: Elements -> States -> Eff (Query ': effs) a -> Eff effs a
runQuery elements statesByElement =
  interpret
    ( \case
        Query selector -> case HashMap.lookup selector elements of
          Just (a : _) -> pure (Just a)
          _ -> pure Nothing
        QueryAll selector -> pure (fromMaybe [] (HashMap.lookup selector elements))
        Get state element ->
          let states = fromMaybe mempty (HashMap.lookup element statesByElement)
           in pure (fromJust (findElementState state states))
    )

runQueryPure :: Elements -> States -> Eff '[Query] a -> a
runQueryPure elements states = run . runQuery elements states

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

assertQuery :: QueryAssertion -> Step -> Result
assertQuery = \(QueryAssertion query' assertion) step ->
  let result' = runQueryPure (queriedElements step) (elementStates step) query'
   in runAssertion assertion result'