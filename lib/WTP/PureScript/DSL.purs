module DSL (
  implies,
  (==>),
  next,
  always,
  Property,
  value,
  textContent,
  enabled,
  checked,
  class StateToField,
  class StatesToRecord,
  Query,
  queryAll,
  queryOne,
  module DSL.Selector,
  module Spec,
  module Prelude
  ) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import DSL.Selector (Selector)
import DSL.Spec (Action(..), Path, Spec, SpecialKey(..), asciiKeyPresses, clicks, foci, keyPresses, specialKeyPress, specialKeyToChar) as Spec
import Type.Prelude (class ListToRow, class TypeEquals)

implies :: Boolean -> Boolean -> Boolean
implies p q = q || not p

infixl 1 implies as ==>

foreign import next :: forall a. a -> a
foreign import always :: Boolean -> Boolean

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
