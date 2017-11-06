module Edit.Command exposing
    ( Command
    , mkMenuItem
    , mkMenuElem

    -- * Obsolete
    -- , MenuCmd(..)
    -- , mkMenuCommand
    )


import Focus as Lens
import Dict as D

import Html as Html
import Html.Attributes as Atts
import Html.Events as Events

import Util as Util
import Edit.Message as Msg
import Edit.Message exposing (Msg(..))


---------------------------------------------------
---------------------------------------------------
-- New version
---------------------------------------------------
---------------------------------------------------


---------------------------------------------------
-- Menu Commands
---------------------------------------------------


-- | Command invocation by a keyboard shortcut
type alias KeyboardShortcut =
    { keyCode : Int
      -- ^ Key code corresponding to the `char`
    , char : Char
      -- ^ The character of the shortcut
    , withCtrl : Maybe Bool
      -- ^ If the command has to be invoked with CTRL pressed; if `Nothing`,
      -- then it can be invoked whether CTRl is pressed or not
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
    }


-- | The void command
void : Command
void = {keyCmd=Nothing, lineCmd=Nothing, menuCmd=Nothing}


-- | Does the command have the given menu name?
hasMenuName : String -> Command -> Bool
hasMenuName name cmd =
    case cmd.menuCmd of
        Nothing -> False
        Just menuCmd -> menuCmd == name


---------------------------------------------------
-- Config
---------------------------------------------------


-- | The global list of commands; using a list because neither `Msg` nor
-- `Command` is comparable...
globalCommands : List (Command, Msg)
globalCommands =
    [ { void
          | keyCmd = Just {keyCode=68, char='d', withCtrl=Just False}
          , menuCmd = Just "Delete" } => Delete
    , { void
          | keyCmd = Just {keyCode=68, char='d', withCtrl=Just True}
          , menuCmd = Just "Delete" } => DeleteTree
    , { void
          | keyCmd = Just {keyCode=68, char='a', withCtrl=Nothing}
          , menuCmd = Just "Add" } => Add
    ]


-- | Create a menu element for a given `Msg`.
mkMenuElem
    : Bool
      -- ^ Is CTRL pressed?
    -> String
      -- ^ The name of the command
    -> Maybe String
      -- ^ The corresponding hint
    -> Maybe (Html.Html Msg)
      -- ^ Doesn't succeed if there is no menu command for the given message
mkMenuElem isCtrl menuName hint =
    let
        -- NOTE: we assume that at most one relevant element should be retrieved
        isRelevant cmd =
            hasMenuName menuName cmd &&
            case cmd.keyCmd of
                Nothing -> True
                Just key -> key.withCtrl == Nothing || key.withCtrl == Just isCtrl
        relevant = List.filter (isRelevant << Tuple.first) globalCommands
    in
        case relevant  of
            [(cmd, msg)] -> Maybe.map (mkMenuItem msg hint) <|
                case cmd.menuCmd of
                    Nothing -> Nothing
                    Just rawName -> Just <|
                        case cmd.keyCmd of
                            Nothing -> plainText rawName
                            Just key ->
                                let charStr = String.fromChar key.char in
                                case String.indexes charStr (String.toLower rawName) of
                                    i :: _ -> emphasizeCtrl (key.withCtrl == Just True) i rawName
                                    _ -> plainText rawName
            _ -> Nothing



-- | Create a menu item for given attributes.
mkMenuItem
    : Msg
       -- ^ The message
    -> Maybe String
       -- ^ Hint
    -> Html.Html Msg
       -- ^ Menu name (in HTML)
    -> Html.Html Msg
mkMenuItem msg hint  name =
    Html.div
        ( [ Events.onClick msg
          , Atts.style
              [ "cursor" => "pointer"
              , "margin-left" => px 10
              , "margin-right" => px 10
              -- | To make the list of commands wrap
              , "display" => "inline-block"
              ]
          ] ++ case hint of
                   Nothing -> []
                   Just x  -> [Atts.title x]
        )
    [ name ]


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)


px : Int -> String
px number =
  toString number ++ "px"


-- ---------------------------------------------------
-- ---------------------------------------------------
-- -- Obsolete
-- ---------------------------------------------------
-- ---------------------------------------------------
--
--
-- ---------------------------------------------------
-- -- Menu Commands
-- ---------------------------------------------------
--
--
-- -- | A specification of a menu command.
-- type MenuCmd
--     = SimpleCmd
--       { msg : Msg
--       , key : Int
--       , char : Char }
--     | DoubleCmd
--       { msg : Msg
--       , msgCtrl : Msg
--       , key : Int
--       , char : Char }
--
--
-- msg : Lens.Focus MenuCmd Msg
-- msg =
--   let
--     get cmd = case cmd of
--       SimpleCmd r -> r.msg
--       DoubleCmd r -> r.msg
--     update f cmd = case cmd of
--       SimpleCmd r -> SimpleCmd {r | msg = f r.msg}
--       DoubleCmd r -> DoubleCmd {r | msg = f r.msg}
--   in
--     Lens.create get update
--
--
-- char : Lens.Focus MenuCmd Char
-- char =
--   let
--     get cmd = case cmd of
--       SimpleCmd r -> r.char
--       DoubleCmd r -> r.char
--     update f cmd = case cmd of
--       SimpleCmd r -> SimpleCmd {r | char = f r.char}
--       DoubleCmd r -> DoubleCmd {r | char = f r.char}
--   in
--     Lens.create get update
--
--
-- -- | A list of triples `(msg, withCtrl, char, key)`, where:
-- -- * `msg` is the message to send when the character `char` is pressed,
-- -- * `key` is the code of the `char`.
-- -- * `withCtrl` is a `Maybe Bool`, whose value is `Nothing` if the command
-- --    should be fired regardeless of whether CTRL is presssed or not, `Just
-- --    True` if CTRL has to be pressed, and `Just False` if it should not be
-- --    pressed.
-- --
-- -- All the commands in this list are supposed to correspond to some menu items.
-- --
-- -- TODO: Should be moved to a configuration module?
-- menuKeyConfig : List MenuCmd
-- menuKeyConfig =
--     let
--         mkSimple msg key char =
--             SimpleCmd {msg=msg, key=key, char=char}
--         mkDouble msg msgCtrl key char =
--             DoubleCmd {msg=msg, msgCtrl=msgCtrl, key=key, char=char}
--     in
--         [ mkDouble Delete DeleteTree 68 'd'
--         , mkSimple Add 65 'a'
--         ]
--
--
-- mkMenuCommand
--     : Bool
--       -- ^ Is CTRL pressed?
--     -> String
--       -- ^ Name of the command
--     -> Msg
--       -- ^ The command
--     -> a
--        -- ^ TODO: A bit stupid, this...
--     -> (Html.Html Msg, Msg, a)
-- mkMenuCommand isCtrl cmdName cmdMsg hint =
--     case List.filter (\cmd -> Lens.get msg cmd == cmdMsg) menuKeyConfig of
--         cmd :: _ ->
--             let
--                 msg =
--                     case cmd of
--                         DoubleCmd x ->
--                             if isCtrl
--                             then x.msgCtrl
--                             else x.msg
--                         SimpleCmd x ->
--                             x.msg
--                 name =
--                     let charStr = String.fromChar <| Lens.get char cmd in
--                     case String.indexes (String.toLower charStr) (String.toLower cmdName) of
--                         i :: _ -> emphasize i cmdName
--                         _ -> plainText cmdName
--             in
--                 (name, msg, hint)
--         [] -> (plainText cmdName, cmdMsg, hint)


---------------------------------------------------
-- Showing text (NOTE: copy in the `View` module)
---------------------------------------------------


plainText : String -> Html.Html Msg
plainText x = Html.text x


emphasize : Int -> String -> Html.Html Msg
emphasize i x =
    Html.span []
        [ Html.text (String.slice 0 i x)
        , Html.u [] [Html.text (String.slice i (i+1) x)]
        , Html.text (String.slice (i+1) (String.length x) x)
        ]


-- | If the flag argument is `True`, then overline the selected character,
-- otherwise like `emphasize`.
emphasizeCtrl : Bool -> Int -> String -> Html.Html Msg
emphasizeCtrl flag i x =
    let
        style =
            if flag
            then [Atts.style ["text-decoration" => "overline"]]
            else [Atts.style ["text-decoration" => "underline"]]
    in
        Html.span []
            [ Html.text (String.slice 0 i x)
            , Html.span style [Html.text (String.slice i (i+1) x)]
            , Html.text (String.slice (i+1) (String.length x) x)
            ]
