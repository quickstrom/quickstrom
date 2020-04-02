{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module WTP.Language where

import qualified Data.Fix                 as Fix
import qualified Data.Functor.Foldable    as Foldable
import qualified Data.Functor.Foldable.TH as TH
import Text.Show.Deriving (deriveShow1)
import Dhall (input, auto, FromDhall, Generic)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(pretty), layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Functor.Classes (Show1)
import Data.Fix (Fix)

data Action = Click { selector :: Text } | Refresh
  deriving (Show, Generic, FromDhall)

data Formula
    = True
    | Not Formula
    | Or Formula Formula
    | Until Formula Formula
  deriving (Show, Generic)

TH.makeBaseFunctor ''Formula

deriving instance Generic (FormulaF a)
deriving instance FromDhall a => FromDhall (FormulaF a)
deriveShow1 ''FormulaF

data Specification formula = Specification 
    { actions :: [Action] 
    , formula :: formula
    }
  deriving (Show, Generic)

deriving instance FromDhall f => FromDhall (Specification f)

embedFormula :: Fix FormulaF -> Formula
embedFormula = Fix.cata Foldable.embed

embedSpecification :: Specification (Fix FormulaF) -> Specification Formula
embedSpecification s = s { formula = embedFormula (formula s) }