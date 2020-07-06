{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module WebCheck.PureScript.Eval.Name where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Language.PureScript.Names as P
import WebCheck.Prelude

newtype Name = Name {unName :: Text}
  deriving (Eq, Show, Ord, Generic, Hashable)

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving (Eq, Show, Ord, Generic, Hashable)

data QualifiedName = QualifiedName (NonEmpty ModuleName) Name
  deriving (Eq, Show, Ord, Generic, Hashable)

fromIdent :: P.Ident -> Name
fromIdent = Name . P.runIdent

fromModuleName :: P.ModuleName -> ModuleName
fromModuleName = ModuleName . P.runModuleName

fromQualifiedIdent :: P.Qualified P.Ident -> Either QualifiedName Name
fromQualifiedIdent (P.Qualified (Just mn) ident) = do
  case NonEmpty.nonEmpty (Text.splitOn "." (P.runModuleName mn)) of
    Just mns -> Left (QualifiedName (map ModuleName mns) (fromIdent ident))
    Nothing -> Right (fromIdent ident)
fromQualifiedIdent (P.Qualified Nothing ident) = Right (fromIdent ident)

toQualifiedIdent :: Either QualifiedName Name -> P.Qualified P.Ident
toQualifiedIdent (Left (QualifiedName mns (Name n))) =
  P.Qualified
    (Just (P.ModuleName (Text.intercalate "." (NonEmpty.toList (map unModuleName mns)))))
    (P.Ident n)
toQualifiedIdent (Right (Name n)) = P.Qualified Nothing (P.Ident n)
