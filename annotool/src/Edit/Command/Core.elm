module Edit.Command.Core exposing
    ( Command
    , void
    -- * JSON
    , commandDecoder
    )


import Focus as Lens
import Dict as D
import Char as Char
import Json.Decode as Decode

import Html as Html
import Html.Attributes as Atts
import Html.Events as Events

import Edit.Message.Core exposing (Msg(..))
import Util


---------------------------------------------------
-- Menu Commands
---------------------------------------------------


-- | Command invocation by a keyboard shortcut
type alias KeyboardShortcut =
    { keyCode : Int
      -- ^ Key code corresponding to the `char`
    , char : Char
      -- ^ The character of the shortcut
    }


-- | A command in the command line mode
type alias LineCommand = String


-- | A menu command
type alias MenuCommand = String
--     { cmdName : String
--     }


-- | A specification of a menu command, which can be used to invoke a particular message.
type alias Command =
    { keyCmd : Maybe KeyboardShortcut
    , lineCmd : Maybe LineCommand
    , menuCmd : Maybe MenuCommand
    , withCtrl : Maybe Bool
      -- ^ If the command has to be invoked with CTRL pressed; if `Nothing`, it
      -- can be invoked whether CTRl is pressed or not; applies to `keyCmd` and
      -- `menuCmd`
    , help : Maybe String
      -- ^ Just a help string
    }


-- | The void command
void : Command
void =
    { keyCmd=Nothing
    , lineCmd=Nothing
    , menuCmd=Nothing
    , withCtrl=Nothing
    , help=Nothing
    }


-- -- | Does the command have the given menu name?
-- hasMenuName : String -> Command -> Bool
-- hasMenuName name cmd =
--     case cmd.menuCmd of
--         Nothing -> False
--         Just menuCmd -> menuCmd == name


---------------------------------------------------
-- JSON decoding
---------------------------------------------------


keyboardShortcutDecoder : Decode.Decoder KeyboardShortcut
keyboardShortcutDecoder =
    Decode.map2 (\keyCode char -> {keyCode=keyCode, char=char})
      (Decode.field "keyCode" Decode.int)
      (Decode.field "char" charDecoder)


commandDecoder : Decode.Decoder Command
commandDecoder =
    let
        mkCmd cmd lin men ctrl hlp =
            { keyCmd = cmd
            , lineCmd = lin
            , menuCmd = men
            , withCtrl = ctrl
            , help = hlp
            }
    in
      Decode.map5 mkCmd
        (Decode.field "keyCmd" (Decode.nullable keyboardShortcutDecoder))
        (Decode.field "lineCmd" (Decode.nullable Decode.string))
        (Decode.field "menuCmd" (Decode.nullable Decode.string))
        (Decode.field "withCtrl" (Decode.nullable Decode.bool))
        (Decode.field "help" (Decode.nullable Decode.string))


charDecoder : Decode.Decoder Char
charDecoder =
    let
        fromText x =
            case String.uncons x of
                Just (c, "") -> Decode.succeed c
                _ -> Decode.fail "not a char"
    in
        Decode.string |> Decode.andThen fromText
