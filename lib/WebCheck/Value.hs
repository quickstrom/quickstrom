{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module WebCheck.Value where

import WebCheck.Element
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import Data.Vector (Vector)
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Vector as Vector
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..))
import GHC.Exts (IsString (..), IsList (..))
import qualified Data.Text as Text

data Value
  = VNull
  | VBool Bool
  | VElement Element
  | VString Text
  | VNumber Scientific
  | VSeq (Vector Value)
  | VSet (HashSet Value)
  | VFunction Function
  deriving (Eq, Show, Generic)

data Function
  = BuiltInFunction BuiltInFunction
  -- TODO: UserDefined
  deriving (Eq, Show, Generic, Hashable)

data BuiltInFunction
  = FAnd
  | FOr
  | FNot
  | FIdentity
  | FIn
  | FLength
  | FFilter
  | FMap
  | FHead
  | FTail
  | FInit
  | FLast
  | FParseNumber
  | FSplitOn
  | FStrip
  deriving (Eq, Show, Generic, Hashable)

instance IsList Value where
  type Item Value = Value
  fromList = VSeq . Vector.fromList
  toList = \case
    VSeq xs -> Vector.toList xs
    _ -> []

instance IsString Value where
  fromString = VString . Text.pack

instance Hashable Value where
  hashWithSalt s = \case
    VNull -> s `hashWithSalt` (0 :: Int)
    VBool b -> s `hashWithSalt` b `hashWithSalt` (1 :: Int)
    VElement e ->s `hashWithSalt` e `hashWithSalt` (2 :: Int)
    VString t -> s `hashWithSalt` t `hashWithSalt` (3 :: Int)
    VNumber n -> s `hashWithSalt` n `hashWithSalt` (4 :: Int)
    VSeq xs -> s `hashWithSalt` (Vector.toList xs) `hashWithSalt` (5 :: Int)
    VSet xs -> s `hashWithSalt` xs `hashWithSalt` (6 :: Int)   
    VFunction n ->   s `hashWithSalt` n `hashWithSalt` (7 :: Int)

instance JSON.FromJSON Value where
  parseJSON = \case
    JSON.Null -> pure VNull
    JSON.String t -> pure (VString t)
    JSON.Bool b -> pure (VBool b)
    JSON.Number n -> pure (VNumber n)
    JSON.Array a -> VSeq <$> traverse parseJSON a
    JSON.Object o -> VElement <$> parseJSON @Element (JSON.Object o)

instance JSON.ToJSON Value where
  toJSON = \case
    VNull -> JSON.Null
    VBool b -> JSON.Bool b
    VElement e -> toJSON e
    VString t -> JSON.String t
    VNumber n -> JSON.Number n
    VSeq xs -> JSON.Array (Vector.map toJSON xs)
    VSet xs -> JSON.Array (Vector.map toJSON (Vector.fromList (HashSet.toList xs)))
    VFunction _ -> JSON.Object mempty
