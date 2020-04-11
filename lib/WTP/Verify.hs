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
  ( verify,
    Result (..),
    Step (..),
    VerifiedStep (..),
    ElementStateValue (..),
    drawVerificationTree,
  )
where

import Control.Monad.Freer
import qualified Data.Bool as Bool
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Formula.Minimal
import WTP.Assertion
import WTP.Query
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

data Result
  = Accepted
  | Rejected Text
  | Undetermined
  deriving (Eq, Show)

instance Semigroup Result where
  Accepted <> _ = Accepted
  Undetermined <> _ = Undetermined
  _ <> r = r

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
        else Rejected (Text.pack (show a) <> " does not equal " <> Text.pack (show expected))
    Contains needle ->
      if needle `Text.isInfixOf` a
        then Accepted
        else Rejected (Text.pack (show a) <> " does not contain " <> Text.pack (show needle))
    Satisfies predicate ->
      if predicate a
        then Accepted
        else Rejected (Text.pack (show a) <> " does not satisfy custom predicate")

data VerifiedStep
  = VerifiedStep
      { step :: Maybe Step,
        stepFormula :: Formula,
        stepResult :: Result
      }
  deriving (Show)

-- TODO: Add writer logging each step, returning it together with result
verify :: Formula -> [Step] -> Tree VerifiedStep
verify spec steps =
  case steps of
    [] -> Tree.Node VerifiedStep {step = Nothing, stepFormula = spec, stepResult = Undetermined} []
    current : rest ->
      let (result, substeps) = case spec of
            True -> (Accepted, [])
            Not p ->
              let r = verify p steps
               in (stepResult (Tree.rootLabel r), [r])
            p `Or` q ->
              let r1 = verify p steps
                  r2 = verify q steps
               in (stepResult (Tree.rootLabel r1) <> stepResult (Tree.rootLabel r2), [r1, r2])
            p `Until` q ->
              let s1 = verify p [current]
                  s2 = verify q rest
                  r = case (stepResult (Tree.rootLabel s1), stepResult (Tree.rootLabel s2)) of
                    (Accepted, Rejected {}) -> stepResult (Tree.rootLabel (verify (p `Until` q) rest))
                    (Accepted, r2) -> r2
                    (_, Accepted) -> Accepted
                    (r1, _) -> r1
               in (r, [s1, s2])
            Assert (QueryAssertion query' assertion) ->
              let result' = runQueryPure (queriedElements current) (elementStates current) query'
               in (runAssertion assertion result', [])
       in Tree.Node VerifiedStep {step = Just current, stepResult = result, stepFormula = spec} substeps

drawVerificationTree :: Tree VerifiedStep -> Tree String
drawVerificationTree =
  ( >>=
      \VerifiedStep {step, stepResult, stepFormula} ->
        let withStepValues :: (Eq k, Hashable k) => (Step -> HashMap k v) -> ((k, v) -> Tree String) -> [Tree String]
            withStepValues field f = map f (HashMap.toList (fromMaybe mempty (field <$> step)))
         in Tree.Node
              "Step"
              [ Tree.Node
                  "Queried elements"
                  ( withStepValues
                      queriedElements
                      ( \(sel, el) ->
                          Tree.Node (show sel) (map (pure . Text.unpack . ref) el)
                      )
                  ),
                Tree.Node
                  "Element states"
                  ( withStepValues
                      elementStates
                      ( \(el, states) ->
                          Tree.Node (Text.unpack (ref el)) (map (pure . drawStateValue) states)
                      )
                  ),
                Tree.Node ("Result = " <> show stepResult) [],
                Tree.Node ("Formula = " <> show stepFormula) []
              ]
  )
  where
    drawStateValue (ElementStateValue state value) = show state <> " = " <> show value
