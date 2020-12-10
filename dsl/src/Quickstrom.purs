module Quickstrom
  ( next
  , always
  , until
  , Property
  , value
  , textContent
  , disabled
  , checked
  , Attribute
  , attribute
  , id
  , CssValue
  , cssValue
  , Query
  , queryAll
  , queryOne
  , class StateToField
  , class StatesToRecord
  , unchanged
  , module Quickstrom.Selector
  , module Spec
  , module Data.HeytingAlgebra
  , module Prelude
  ) where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe)
import Data.HeytingAlgebra (implies)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Quickstrom.Selector (Selector)
import Quickstrom.Spec (Actions, ProbabilisticAction, BaseAction(..), Action(..), Path, SpecialKey(..), asciiKeyPresses, clicks, foci, focus, keyPress, specialKeyPress) as Spec
import Type.Prelude (class ListToRow, class TypeEquals)

-- ## Temporal operators

foreign import _next :: forall a. a -> a

foreign import _always :: Boolean -> Boolean

foreign import _until :: Boolean -> Boolean -> Boolean

next :: forall a. a -> a
next p = _next p

always :: Boolean -> Boolean
always p = _always p

until :: Boolean -> Boolean -> Boolean
until p q = _until p q

-- ## Properties
foreign import data Property :: Symbol -> Type -> Type

foreign import _property :: forall name typ. String -> Property name typ

property :: forall name typ. IsSymbol name => SProxy name -> Property name typ
property name = _property (reflectSymbol name)

value :: Property "value" String
value = property (SProxy :: SProxy "value")

textContent :: Property "textContent" String
textContent = property (SProxy :: SProxy "textContent")

disabled :: Property "disabled" Boolean
disabled = property (SProxy :: SProxy "disabled")

checked :: Property "checked" Boolean
checked = property (SProxy :: SProxy "checked")

-- ## Attributes
foreign import data Attribute :: Symbol -> Type

foreign import _attribute :: forall name. String -> Attribute name

attribute :: forall name. IsSymbol name => SProxy name -> Attribute name
attribute name = _attribute (reflectSymbol name)

id :: Attribute "id"
id = attribute (SProxy :: SProxy "id")

-- ## CSS values
foreign import data CssValue :: Type

foreign import cssValue :: String -> CssValue

-- ## Queries
class StateToField (state :: Type) (field :: Type) | state -> field

instance stateToFieldProperty :: TypeEquals ptype typ => StateToField (Property name ptype) typ

instance stateToFieldAttribute :: StateToField (Attribute name) (Maybe String)

instance stateToFieldCssValue :: StateToField CssValue String

class StatesToRecord (states :: RowList) (out :: RowList) | states -> out

instance statesToRecordCons ::
  ( StatesToRecord tail tail'
  , StateToField state field
  ) =>
  StatesToRecord (Cons name state tail) (Cons name field tail')

instance statesToRecordNil :: StatesToRecord Nil Nil

type Query f
  = forall states sl record rl.
    StatesToRecord sl rl =>
    RowToList states sl =>
    ListToRow rl record =>
    Selector ->
    Record states ->
    f (Record record)

foreign import _queryAll :: forall states record. Selector -> Record states -> Array (Record record)

queryAll :: Query Array
queryAll selector states = _queryAll selector states

queryOne :: Query Maybe
queryOne selector = head <<< queryAll selector

 -- ## State

unchanged :: forall a. Eq a => a -> Boolean
unchanged x = x == next x
