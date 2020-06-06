module WebCheck.DSL (
  next,
  always,
  trace,
  traceShow,
  traceShowLabelled,
  Property,
  value,
  textContent,
  enabled,
  checked,
  queryAll,
  queryOne,
  Query,
  class StateToField,
  class StatesToRecord,
  module WebCheck.DSL.Selector,
  module Spec,
  module Data.HeytingAlgebra,
  module Prelude
  ) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe)
import Data.HeytingAlgebra (implies)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import WebCheck.DSL.Selector (Selector)
import WebCheck.DSL.Spec (Action, Path, Spec, SpecialKey(..), asciiKeyPresses, clicks, foci, focus, keyPress, specialKeyPress) as Spec
import Type.Prelude (class ListToRow, class TypeEquals)

foreign import next :: forall a. a -> a
foreign import always :: Boolean -> Boolean

foreign import trace :: forall a. String -> a -> a

traceShow :: forall a. Show a => a -> a
traceShow x = trace (show x) x

traceShowLabelled :: forall a. Show a => String -> a -> a
traceShowLabelled lbl x = trace (lbl <> ": " <> show x) x

-- ## Properties

foreign import data Property :: Symbol -> Type -> Type

foreign import _property :: forall name typ. String -> Property name typ

property :: forall name typ. IsSymbol name => SProxy name -> Property name typ
property name = _property (reflectSymbol name)

value :: Property "value" String
value = property (SProxy :: SProxy "value")

textContent :: Property "textContent" String
textContent = property (SProxy :: SProxy "textContent")

enabled :: Property "enabled" Boolean
enabled = property (SProxy :: SProxy "enabled")

checked :: Property "checked" Boolean
checked = property (SProxy :: SProxy "checked")

-- ## Attributes

foreign import data Attribute :: Symbol -> Type

foreign import _attribute :: forall name. String -> Attribute name

attribute :: forall name. IsSymbol name => SProxy name -> Attribute name
attribute name = _attribute (reflectSymbol name)

id :: Attribute "id"
id = attribute (SProxy :: SProxy "id")

-- ## Queries

class StateToField (state :: Type) (field :: Type) | state -> field

instance stateToFieldProperty :: TypeEquals ptype typ => StateToField (Property name ptype) typ

instance stateToFieldAttribute :: StateToField (Attribute name) String

class StatesToRecord (states :: RowList) (out :: RowList) | states -> out

instance statesToRecordCons
         :: ( StatesToRecord tail tail'
            , StateToField state field
            )
         => StatesToRecord (Cons name state tail) (Cons name field tail')

instance statesToRecordNil :: StatesToRecord Nil Nil

type Query f =
  forall states sl record rl
   . StatesToRecord sl rl 
  => RowToList states sl
  => ListToRow rl record
  => Selector 
  -> Record states 
  -> f (Record record)

foreign import _queryAll :: forall states record. Selector -> Record states -> Array (Record record)

queryAll :: Query Array
queryAll selector states = _queryAll selector states

queryOne :: Query Maybe
queryOne selector = head <<< queryAll selector

-- ## Spec
