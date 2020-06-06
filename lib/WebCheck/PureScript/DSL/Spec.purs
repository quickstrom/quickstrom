module WebCheck.DSL.Spec
 ( Path
 , Action
 , Actions
 , clicks
 , focus
 , foci
 , keyPress
 , asciiKeyPresses
 , SpecialKey(..)
 , specialKeyPress
 , Spec
 )
where

import Prelude

import WebCheck.DSL.Selector (Selector)
import Data.Array (range)
import Data.Char (fromCharCode)
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

type Path = String

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path

type Actions = Array Action

clicks :: Actions
clicks = [ Click "button", Click "input[type=button]", Click "a" ]

focus :: Selector -> Action
focus = Focus

foci :: Actions
foci = [ Focus "input", Focus "textarea" ]

keyPress :: Char -> Action
keyPress = KeyPress

asciiKeyPresses :: Actions
asciiKeyPresses = KeyPress <<< unsafePartial fromJust <<< fromCharCode <$> range 32 126

specialKeyPress :: SpecialKey -> Action
specialKeyPress specialKey =
  KeyPress (specialKeyToChar specialKey)

type Spec = { origin :: Path, readyWhen :: Selector, proposition :: Boolean, actions :: Actions }

data SpecialKey
  = KeyAdd
  | KeyAlt
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyBackspace
  | KeyCancel
  | KeyClear
  | KeyCommand
  | KeyControl
  | KeyDecimal
  | KeyDelete
  | KeyDivide
  | KeyDown
  | KeyEnd
  | KeyEnter
  | KeyEquals
  | KeyEscape
  | KeyF1
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyHelp
  | KeyHome
  | KeyInsert
  | KeyLeft
  | KeyLeftAlt
  | KeyLeftControl
  | KeyLeftShift
  | KeyMeta
  | KeyMultiply
  | KeyNull
  | KeyNumpad0
  | KeyNumpad1
  | KeyNumpad2
  | KeyNumpad3
  | KeyNumpad4
  | KeyNumpad5
  | KeyNumpad6
  | KeyNumpad7
  | KeyNumpad8
  | KeyNumpad9
  | KeyPageDown
  | KeyPageUp
  | KeyPause
  | KeyReturn
  | KeyRight
  | KeySemicolon
  | KeySeparator
  | KeyShift
  | KeySpace
  | KeySubtract
  | KeyTab
  | KeyUp

derive instance eqSpecialKey :: Eq SpecialKey

derive instance genericSpecialKey :: Generic SpecialKey _

instance showSpecialKey :: Show SpecialKey where
  show = genericShow

instance ordSpecialKey :: Ord SpecialKey where
  compare = genericCompare

instance enumSpecialKey :: Enum SpecialKey where
    pred = genericPred
    succ = genericSucc

specialKeyToChar :: SpecialKey -> Char
specialKeyToChar = case _ of
  KeyAdd -> '\xe025'
  KeyAlt -> '\xe00a'
  KeyArrowDown -> '\xe015'
  KeyArrowLeft -> '\xe012'
  KeyArrowRight -> '\xe014'
  KeyArrowUp -> '\xe013'
  KeyBackspace -> '\xe003'
  KeyCancel -> '\xe001'
  KeyClear -> '\xe005'
  KeyCommand -> '\xe03d'
  KeyControl -> '\xe009'
  KeyDecimal -> '\xe028'
  KeyDelete -> '\xe017'
  KeyDivide -> '\xe029'
  KeyDown -> '\xe015'
  KeyEnd -> '\xe010'
  KeyEnter -> '\xe007'
  KeyEquals -> '\xe019'
  KeyEscape -> '\xe00c'
  KeyF1 -> '\xe031'
  KeyF10 -> '\xe03a'
  KeyF11 -> '\xe03b'
  KeyF12 -> '\xe03c'
  KeyF2 -> '\xe032'
  KeyF3 -> '\xe033'
  KeyF4 -> '\xe034'
  KeyF5 -> '\xe035'
  KeyF6 -> '\xe036'
  KeyF7 -> '\xe037'
  KeyF8 -> '\xe038'
  KeyF9 -> '\xe039'
  KeyHelp -> '\xe002'
  KeyHome -> '\xe011'
  KeyInsert -> '\xe016'
  KeyLeft -> '\xe012'
  KeyLeftAlt -> '\xe00a'
  KeyLeftControl -> '\xe009'
  KeyLeftShift -> '\xe008'
  KeyMeta -> '\xe03d'
  KeyMultiply -> '\xe024'
  KeyNull -> '\xe000'
  KeyNumpad0 -> '\xe01a'
  KeyNumpad1 -> '\xe01b'
  KeyNumpad2 -> '\xe01c'
  KeyNumpad3 -> '\xe01d'
  KeyNumpad4 -> '\xe01e'
  KeyNumpad5 -> '\xe01f'
  KeyNumpad6 -> '\xe020'
  KeyNumpad7 -> '\xe021'
  KeyNumpad8 -> '\xe022'
  KeyNumpad9 -> '\xe023'
  KeyPageDown -> '\xe00f'
  KeyPageUp -> '\xe00e'
  KeyPause -> '\xe00b'
  KeyReturn -> '\xe006'
  KeyRight -> '\xe014'
  KeySemicolon -> '\xe018'
  KeySeparator -> '\xe026'
  KeyShift -> '\xe008'
  KeySpace -> '\xe00d'
  KeySubtract -> '\xe027'
  KeyTab -> '\xe004'
  KeyUp -> '\xe013'
