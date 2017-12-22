module Edit.View exposing (view)


import Char
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Set as S
import Dict as D
import List as L
import Maybe as Maybe
import Tuple exposing (first, second)
import Mouse exposing (Position)
import Focus as Lens
import Focus exposing ((=>))

import Rose as R
import Util as Util
import Config as Cfg
import Edit.Anno as Anno
import Edit.Anno.Core as Anno
import Edit.Config as AnnoCfg
import Edit.Model as M
import Edit.Core as C
import Edit.Command as Cmd
import Edit.Message as Msg
import Edit.Message.Core exposing (Msg(..))
import Edit.Message.Core as Msg
import Edit.Popup as Popup
import Server
import Server.Core as Server

import Edit.View.Tree as Tree


-- | The main view function.
view : M.Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "width" :> "100%"
        , "height" :> "100%"
        ]

    -- register the keyboard events from top-level
    , Atts.attribute "tabindex" "1"
    , globalKeyDown model
    , globalKeyUp
    ]
    ( [ stylesheet
      , viewWindow C.Top model
      ] ++ viewSideWindow C.Top model ++
      [ viewWindow C.Bot model
      ] ++ viewSideWindow C.Bot model
        ++ viewLinks model
        ++ viewPopups model
    )


stylesheet =
  let
    tag = "link"
    attrs =
      [ Atts.attribute "rel" "stylesheet"
      , Atts.attribute "property" "stylesheet"
      -- , attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      , Atts.attribute "href" "public/css/style.css"
      -- , Atts.attribute "href" "style.css"
      ]
    children = []
  in
    Html.node tag attrs children


-- | The view of the top/bottom window.
viewWindow : C.Focus -> M.Model -> Html.Html Msg
viewWindow win model =
  Html.div
  [ backMouseDown win
  , winOnFocus win

  -- , topOnResize
--   , case win of
--       C.Top -> Atts.autofocus True
--       C.Bot -> Atts.autofocus False

--   , case win of
--       C.Top -> Atts.id "top"
--       C.Bot -> Atts.id "bot"

  , Atts.id <| case win of
      C.Top -> Cfg.windowName True
      C.Bot -> Cfg.windowName False

  -- @tabindex required to make the div propagate the keyboard events
  -- (see the `view` function)
  , Atts.attribute "tabindex" "1"

  , Atts.style
    [ "position" :> "absolute"
    -- , "width" :> (toString (100 - Cfg.sideSpace) ++ "%") -- "100%"
    , "width" :> (toString model.dim.widthProp ++ "%")
    , "height" :> case win of
        C.Top -> toString model.dim.heightProp ++ "%"
        C.Bot -> toString (100 - model.dim.heightProp) ++ "%"
    , case win of
        C.Top -> "top" :> "0"
        C.Bot -> "bottom" :> "0"
    -- Overflow is a very important attribute (it makes the scrollbars to appear
    -- if set to "auto") which makes sure that the trees do not go beyond the
    -- specified subwindows.  We set to "hidden" so that tracing links is easy
    -- (otherwise, special care has to be taken w.r.t. scrollbars).
    , "overflow" :> "hidden"
    , "background-color" :> backColor win model
    , "opacity" :> "1.0"
    -- , "border" :> "1px black solid"

    -- z-index important because of its interactions with how the edges are
    -- drawn *and* with popup windows
    , "z-index" :> "-1"

    -- make the outline invisible (the fact that the window is in focus is
    -- visible anyway)
    , "outline" :> "0"
    ]
  ]

  <|

  [ Tree.viewTree win model
  , viewBottomLine win model ]
  ++ if (win == C.Bot && model.dim.heightProp <= 0) ||
        (win == C.Top && model.dim.heightProp > 0)
     then [viewMenu model] -- model.fileId]
     else []


-- | Determine the background color.
backColor : C.Focus -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"


-- -- | Retrieve the token map (position -> token) for a given partition.
-- tokenMap : C.PartId -> M.Model -> Int -> M.Token
-- tokenMap treeId model =
--     let
--         mkDict pos xs =
--             case xs of
--                 [] -> D.empty
--                 hd :: tl -> D.insert pos hd <| mkDict (pos + 1) tl
--         sentDict = mkDict 0 <| M.getSent treeId model
--     in
--         \i -> case D.get i sentDict of
--                   Nothing -> M.emptyToken
--                   Just x  -> x


---------------------------------------------------
-- Bottom line
---------------------------------------------------


-- | View the bottom line: either tree ID or the command.
viewBottomLine : C.Focus -> M.Model -> Html.Html Msg
viewBottomLine win model =
  case (model.command, win == model.focus) of
    (Just cmd, True) -> viewCommand cmd win model
    _ -> viewTreeId win model


viewTreeId : C.Focus -> M.Model -> Html.Html Msg
viewTreeId win model =
  let
    currentFileId = M.getFileId win model
    allFileIds = L.map Tuple.first model.fileList

    txt0 = toString (M.treePos win model)
      ++ "/"
      ++ toString (M.treeNum win model)

    fileIDs = bracket "(" ")" <|
      Util.intercalate (Html.text " ")
      (L.map mkID allFileIds)
    mkID fid =
      if fid == currentFileId
      then Html.text (C.encodeFileId fid)
      else Html.span
          [ Atts.style ["color" :> "gray", "cursor" :> "pointer"]
          , Atts.title "Click to swap the file"
          , Events.onClick (SwapFileTo fid) ]
          [ Html.text (C.encodeFileId fid) ]
    bracket x y xs = Html.text x :: xs ++ [Html.text y]

    txt2 =
      if model.ctrl
      then " CTRL"
      else ""

    span = Html.span []
      [ Html.text (txt0 ++ " ")
      , Html.span [] fileIDs
      , Html.text txt2
      ]

  in

    Html.div bottomStyle [span]


viewCommand : String -> C.Focus -> M.Model -> Html.Html Msg
viewCommand pref win model =
  let
    cmdLst = Msg.cmdsWithPrefix model.annoConfig pref
    cmdStr = String.trim
             <| String.concat
             <| List.map (\cmd -> String.cons ' ' cmd)
             <| List.sort
             <| cmdLst
    txt = ":" ++ pref ++ " (" ++ cmdStr ++ ")"
  in
    Html.div
      bottomStyle
      [ Html.text txt ]


bottomStyle : List (Html.Attribute msg)
bottomStyle =
    [ Atts.class "noselect"
    , Atts.style
        [ "position" :> "absolute"
        -- , "width" :> "5%"
        -- , "height" :> "5%"
        , "bottom" :> px 10
        , "left" :> px 10
        ]
    ]


---------------------------------------------------
-- Popup
---------------------------------------------------


type alias PopupCommand = (String, Msg)


viewPopups : M.Model -> List (Html.Html Msg)
viewPopups model =
    case model.popup of
        Nothing -> []
        Just (Popup.Files x) -> [viewPopupFiles x]
        Just (Popup.Info x) -> [viewPopupInfo x]
        Just (Popup.Split x) -> [viewPopupSplit x]


viewPopupInfo : String -> Html.Html Msg
viewPopupInfo info =
    viewPopupGen 125 125 info [("OK", QuitPopup)] 0


viewPopupFiles : Maybe (List C.FileId) -> Html.Html Msg
viewPopupFiles maybeFiles =
    case maybeFiles of
        Nothing ->
            viewPopupGen 200 125 "Comparing file(s) with their database versions..."
                [ ("Cancel", QuitPopup)
                ] 0
        Just [] ->
            viewPopupGen 200 125 "All changes saved. Do you wish to exit?"
                [ ("Yes", Many [QuitPopup, Files])
                , ("No", QuitPopup)
                ] 0
        Just xs ->
            let msg0 = "The following files differ from their versions in the database:"
                msg1 = String.join ", " <| L.map C.encodeFileId xs
                msg2 = "Are you sure you want to exit?"
                msg = msg0 ++ " " ++ msg1 ++ ". " ++ msg2
            in  viewPopupGen 200 125  msg
                [ ("Yes", Many [QuitPopup, Files])
                , ("No", QuitPopup)
                ] 1


viewPopupSplit : Popup.SplitPopup -> Html.Html Msg
viewPopupSplit r =
    viewPopupSplitGen 125 125 r
        [ ("OK", Many [SplitFinish r.split, QuitPopup])
        , ("Cancel", QuitPopup)
        ] 0


-- | Generic popup.
viewPopupGen
    : Int
    -- ^ Font size of the popup message (in percentage)
    -> Int
    -- ^ Font size of the commands (in percentage)
    -> String
    -- ^ The message
    -> List PopupCommand
    -- ^ A list of commands that the user can perform
    -> Int
    -- ^ The position of the default command
    -> Html.Html Msg
viewPopupGen msgSize cmdSize msg commandList defaultCommand =
    let
        textElem = popupTextElem msgSize
        div = Html.div
              [] -- [Atts.id Cfg.popupDivTemp]
              [textElem msg, Html.hr [] []]
    in
        -- viewPopupMostGen cmdSize (textElem msg) commandList defaultCommand
        viewPopupMostGen cmdSize div commandList defaultCommand


-- | Splitting popup
viewPopupSplitGen
    : Int
    -- ^ Font size of the popup message (in percentage)
    -> Int
    -- ^ Font size of the commands (in percentage)
    -> Popup.SplitPopup
    -- ^ The message
    -> List PopupCommand
    -- ^ A list of commands that the user can perform
    -> Int
    -- ^ The position of the default command
    -> Html.Html Msg
viewPopupSplitGen msgSize cmdSize spl commandList defaultCommand =

    let

        option evVal val =
            Html.option
                [ Atts.value (toString val)
                , Atts.selected (val == evVal) ]
            [ Html.text (toString val) ]
        setMsg str =
            let x = String.toInt str |> Result.toMaybe |> Maybe.withDefault 0
            in  SplitChange x
        splitButton val beg end =
            Html.select
                [ Events.on "change" (Decode.map setMsg Events.targetValue)
                , Atts.id Cfg.splitSelectName
                , Atts.autofocus True -- ^ doesn't work?
                , Atts.style
                    [ "cursor" :> "pointer"
                    , "font-size" :> ps msgSize ]
                ]
                ( List.map (option val) (List.range beg end) )

        textElem = popupTextElem msgSize
        textCell txt = Html.td [] [textElem txt]
        tab =
            Html.table []
                [ Html.tr []
                      [ textCell "split: "
                      , splitButton spl.split 0 (String.length spl.word)
                      ]
                -- , Html.tr [] [textCell "from: ", textCell spl.word]
                , Html.tr []
                      [ textCell "first: "
                      , textCell (String.left spl.split spl.word)
                      ]
                , Html.tr []
                      [ textCell "second: "
                      , textCell (String.dropLeft spl.split spl.word)
                      ]
                ]

    in

        viewPopupMostGen cmdSize (Html.div [] [tab, Html.hr [] []]) commandList defaultCommand


popupTextElem
    : Int
    -- ^ Font size of the popup message (in percentage)
    -> String
    -- ^ Text message
    -> Html.Html Msg
popupTextElem msgSize txt =
    Html.span
        [ Atts.class "noselect"
        , Atts.style
            [ "cursor" :> "default"
            , "margin" :> px 5
            -- , "font-size" :> "125%" ]
            , "font-size" :> ps msgSize ]
        ]
        [ Html.text txt ]


-- | Doubly generic popup.
viewPopupMostGen
     : Int
    -- ^ Font size of the commands (in percentage)
    -> Html.Html Msg
    -- ^ Main Popup HTML
    -> List PopupCommand
    -- ^ A list of commands that the user can perform
    -> Int
    -- ^ The position of the default command
    -> Html.Html Msg
viewPopupMostGen cmdSize popupHtml commandList defaultCommand =

    let

        textButton txt action isDefault =
            let
                maybeID =
                    if isDefault
                    then [Atts.id Cfg.popupDivTemp]
                    else []
            in
                Html.button (
                    [ Atts.class "noselect"
                    , Events.onClick action
                    , Atts.style
                        [ "cursor" :> "pointer"
                        , "margin-left" :> px 5
                        , "margin-right" :> px 5
                        , "font-size" :> ps cmdSize ]
                    ] ++ maybeID )
                    [ Html.text txt ]

        commandListWithIDs =
            L.map2 (,) commandList (L.range 0 (L.length commandList))

        buttons =
            L.map
                (\((cmd, msg), cmdID) -> textButton cmd msg (cmdID == defaultCommand))
                commandListWithIDs

        popupDiv =
            Html.div
                [ Atts.style
                  [ "position" :> "absolute"
                  , "display" :> "inline-block"
                  , "left" :> "50%"
                  -- , "left" :> (toString (model.dim.widthProp // 2) ++ "%")
                  , "top" :> "50%"
                  , "-webkit-transform" :> "translate(-50%, -50%)"
                  , "transform" :> "translate(-50%, -50%)"
                  -- , "background-color" :> "#48e"
                  , "text-align" :> "center"
                  -- , "border-radius" :> "10%" -- "4px"
                  ]
                ]
                [ popupHtml
                , Html.div [] buttons
                ]
        keyboardHandler code = case code of
--             -- Enter (JW: better when handled automatically)
--             13 -> case L.head (L.drop defaultCommand commandList) of
--                 Nothing -> Msg.dummy
--                 Just (_, msg) -> msg
            -- Escape
            27 -> QuitPopup
            _  -> Msg.dummy
    in
        Html.div
            [ Atts.class "popup"
            , popupKeyDownEvents keyboardHandler

            -- @tabindex required to make the div propagate the keyboard events
            -- (otherwise they are only propagated when some other HTML items --
            -- e.g. buttons -- are selected)
            , Atts.attribute "tabindex" "1"

            -- make the outline invisible (the fact that the window is in focus is
            -- visible anyway)
            -- , "outline" :> "0"

            , Atts.style
                  [ "position" :> "absolute"
                  , "width" :> "100%"
                  , "height" :> "100%"
                  , "opacity" :> "0.85"
                  -- , "background-color" :> "#e84"
                  ]
            ]
            [ popupDiv
            ]


---------------------------------------------------
-- Menu
---------------------------------------------------


-- viewMenu
--     : M.Model
--     -- : String -- File name
--     -> Html.Html Msg
-- viewMenu model = -- fileName =
--   let
--
--     menuElem onClick pos txt = Html.div
--       [ Atts.class "noselect"
--       , Events.onClick onClick
--       , Atts.style
--         [ "position" :> "absolute"
--         , "top" :> px 10
--         , "left" :> px pos
--         , "cursor" :> "pointer"
--         ]
--       ]
--       -- [ Html.text txt ]
--       [ txt ]
--
--     -- initPos = (String.length fileName * 6) + 20
--
--   in
--
-- --     Html.div []
-- --       [ menuElem Msg.dummy 10 ("[" ++ fileName ++ "]")
-- --       , menuElem Files (initPos + 60) "Menu"
-- --       , menuElem SaveFile (initPos + 120) "Save" ]
--
--     Html.div []
--       [ menuElem (Popup Popup.Files Nothing) 10 (plainText "Menu")
--       , menuElem SaveFile 70 (plainText "Save")
--       -- , menuElem EditLabel 120 (plain "Edit")
--       , menuElem MkEvent 120 (emphasize 1 "Event")
--       , menuElem MkSignal 180 (emphasize 0 "Signal")
--       , menuElem MkTimex 240 (emphasize 0 "Timex")
--       , menuElem (Many []) 320 (plainText <| toString model.ctrl)
--       ]


-- viewMenu
--     : M.Model
--     -- : String -- File name
--     -> Html.Html Msg
-- viewMenu model = -- fileName =
--   let
--
--     menuElem onClick txt hint =
--         Html.div
--           ( [ Events.onClick onClick
--             , Atts.style
--                 [ "cursor" :> "pointer"
--                 , "margin-left" :> px 10
--                 , "margin-right" :> px 10
--                 -- | To make the list of commands wrap
--                 , "display" :> "inline-block"
--                 ]
--             ] ++ case hint of
--                      Nothing -> []
--                      Just x  -> [Atts.title x]
--           )
--         [ txt ]
--
--     isCtrl = if model.ctrl then "CTRL" else ""
--     annoLevel = toString model.annoLevel
--     mkMenuItem = Cmd.mkMenuCommand model.ctrl
--
--     segmentationCommands =
--       [ ( plainText "Split tree", SplitTree
--         , Just "Split the current tree into several sentences at the selected terminal nodes" )
--       , ( plainText "Split word", SplitBegin
--         , Just "Split the current word in two" )
--       , ( plainText "Concatenate words", ConcatWords
--         , Just "Concatenate the tokens corresponding to selected terminals" )
--       ]
--
--     syntaxCommands =
--       [ ( plainText "POS tag and parse", ParseSent Server.Stanford
--         , Just "POS tag and parse the current sentence with the Stanford parser" )
--       , ( plainText "Parse", ParseSentPos Server.Stanford
--         , Just "Parse the current sentence with the Stanford parser, without changing the selected part-of-speech tags" )
--       , mkMenuItem "Delete" Delete <|
--           Just "Delete the selected nodes and, with CTRL, their subtrees"
--       , mkMenuItem "Add node" Add <|
--           Just "Add (a) new node(s) over the selected node(s)"
--       ]
--
--     temporalCommands =
--       [ (emphasize 1 "Event", MkEvent, Just "Mark (or unmark) the selected node as event")
--       , (emphasize 0 "Signal", MkSignal, Just "Mark (or unmark) the selected node as signal")
--       , (emphasize 0 "Timex", MkTimex, Just "Mark (or unmark) the selected node as timex") ]
--
--     mkCommands = List.map <| \(html, event, hint) -> menuElem event html hint
--
--   in
--
--     Html.div
--       [ Atts.class "noselect"
--       , Atts.style
--         [ "position" :> "absolute"
--         , "top" :> px 10
--         , "left" :> px 5
--         ]
--       ] <| mkCommands <|
--         [ (plainText "Menu", Popup Popup.Files Nothing, Just "Go to the main menu")
--         , (plainText "Save", SaveFile, Just "Save the entire current file")
--         , (plainText annoLevel, ChangeAnnoLevel, Just "Change the annotation level commands") ] ++
--         ( if model.annoLevel == M.Temporal
--           then temporalCommands
--           else if model.annoLevel == M.Segmentation
--                then segmentationCommands
--                else syntaxCommands ) ++
--         [ (plainText isCtrl, Dummy, Just "Is CTRL down") ]


viewMenu
    : M.Model
    -- : String -- File name
    -> Html.Html Msg
viewMenu model = -- fileName =
  let

--     menuElem onClick txt hint =
--         Html.div
--           ( [ Events.onClick onClick
--             , Atts.style
--                 [ "cursor" :> "pointer"
--                 , "margin-left" :> px 10
--                 , "margin-right" :> px 10
--                 -- | To make the list of commands wrap
--                 , "display" :> "inline-block"
--                 ]
--             ] ++ case hint of
--                      Nothing -> []
--                      Just x  -> [Atts.title x]
--           )
--         [ txt ]

    annoLevel = toString model.annoLevel
--     annoLevelPadded =
--         let
--             len = String.length (toString M.Segmentation) + 2
--         in
--             String.pad len '-' annoLevel

--     mkMenuItem = Cmd.mkMenuCommand model.ctrl
--
--     segmentationCommands =
--       [ ( plainText "Split tree", SplitTree
--         , Just "Split the current tree into several sentences at the selected terminal nodes" )
--       , ( plainText "Split word", SplitBegin
--         , Just "Split the current word in two" )
--       , ( plainText "Concatenate words", ConcatWords
--         , Just "Concatenate the tokens corresponding to selected terminals" )
--       ]
--
--     syntaxCommands =
--       [ ( plainText "POS tag and parse", ParseSent Server.Stanford
--         , Just "POS tag and parse the current sentence with the Stanford parser" )
--       , ( plainText "Parse", ParseSentPos Server.Stanford
--         , Just "Parse the current sentence with the Stanford parser, without changing the selected part-of-speech tags" )
--       , mkMenuItem "Delete" Delete <|
--           Just "Delete the selected nodes and, with CTRL, their subtrees"
--       , mkMenuItem "Add node" Add <|
--           Just "Add (a) new node(s) over the selected node(s)"
--       ]
--
--     temporalCommands =
--       [ (emphasize 1 "Event", MkEvent, Just "Mark (or unmark) the selected node as event")
--       , (emphasize 0 "Signal", MkSignal, Just "Mark (or unmark) the selected node as signal")
--       , (emphasize 0 "Timex", MkTimex, Just "Mark (or unmark) the selected node as timex") ]

    mkMenuElem = Cmd.mkMenuElem model.annoConfig model.ctrl
    segmentationCommands = Util.catMaybes <| List.map mkMenuElem
        [ ParseRaw False, ParseRaw True
        , SplitTree, SplitBegin
        , ConcatWords
        ]
    syntaxCommands = Util.catMaybes <| List.map mkMenuElem
        [ Dummy
        , ParseSent Server.Stanford
        , ParseSentPos Server.Stanford
        , Delete, DeleteTree, Add
        ]
    temporalCommands = Util.catMaybes <| List.map mkMenuElem
        [ MkEntity "Event"
        , MkEntity "Signal"
        , MkEntity "Timex"
        ]

    levelElem level = Html.div
        [ Events.onClick (ChangeAnnoLevelTo level)
        , Atts.style Cmd.menuItemStyle
        , Atts.title <| "Switch to the " ++ toString level ++ " annotation level" ]
        [ if model.annoLevel == level
          then Html.text (toString level)
          else Html.span [Atts.style ["color" :> "gray"]] [Html.text (toString level)]
        ]
    levelPart = Html.span [] <|
        -- Util.intercalate (Html.text " ")
        [ Html.text "|"
        , levelElem C.Segmentation
        , levelElem C.Syntax
        , levelElem C.Temporal
        , Html.text "|"
        ]

  in

    Html.div
      [ Atts.class "noselect"
      , Atts.style
        [ "position" :> "absolute"
        , "top" :> px 10
        , "left" :> px 5
        ]
      ] <|
        [ Cmd.mkMenuItem Quit
              (Just "Go to the main menu")
              (plainText "Menu")
--         , Html.div
--               [ Atts.style Cmd.menuItemStyle
--               , Atts.title "Go to the main menu" ]
--               [ Html.a
--                     [Atts.attribute "href" "."]
--                     [plainText "Menu"]
--               ]
        , Cmd.mkMenuItem
              SaveFile
              (Just "Save the entire current file")
              (plainText "Save")
        , levelPart
--         , Cmd.mkMenuItem
--               ChangeAnnoLevel
--               (Just "Click to change the annotation level")
--               (plainText <| "| " ++ annoLevel ++ " |")
        ] ++
        ( if model.annoLevel == C.Temporal
          then temporalCommands
          else if model.annoLevel == C.Segmentation
               then segmentationCommands
               else syntaxCommands )
        -- [ Cmd.mkMenuItem Dummy (Just "Is CTRL down") (plainText isCtrl) ]


---------------------------------------------------
-- Side windows
---------------------------------------------------


-- | The view of a side window.
viewSideWindow : C.Focus -> M.Model -> List (Html.Html Msg)
viewSideWindow focus model =
    let
        -- theSide = (M.selectWin focus model).side
        theSide = (Lens.get (M.workspaceLens focus) model).side
        context = viewSideContext (theSide == M.SideContext) focus model
        edit = viewSideEdit (theSide == M.SideEdit) focus model
        log = viewSideLog (theSide == M.SideLog) focus model
    in
        [context, edit, log]


viewSideDiv
    : Bool        -- ^ Visible?
    -> C.Focus
    -> M.Model
    -> List (Html.Html Msg)
    -> Html.Html Msg
viewSideDiv visible win model children =
  let
    dim = model.dim
    div = Html.div
      [ Atts.style
        [ "position" :> "absolute"
        -- , "width" :> (toString Cfg.sideSpace ++ "%")
        , "width" :> (toString (100 - dim.widthProp) ++ "%")
        -- , "height" :> "50%"
        , "height" :> case win of
            C.Top -> toString dim.heightProp ++ "%"
            C.Bot -> toString (100 - dim.heightProp) ++ "%"
        , "right" :> "0"
        , case win of
            C.Top -> "top" :> "0"
            C.Bot -> "bottom" :> "0"
        , "overflow" :> "auto"
        -- make the (focus-related) outline invisible
        , "outline" :> "0"
        -- z-index important because of its interactions with popup windows;
        -- note that it creates a "stacking context" for all the children HTML
        -- elements
        -- (https://philipwalton.com/articles/what-no-one-told-you-about-z-index/)
        , "z-index" :> "-1"
        , "display" :> if visible then "block" else "none"
        ]
      -- @tabindex required to make the div propagate the keyboard events
      -- (see the `view` function)
      , Atts.attribute "tabindex" "1"
      , Atts.id <| Cfg.sideDivName <|
          case win of
              C.Top -> True
              C.Bot -> False
      ]
    topChildren = [viewSideMenu win model]
  in
    -- `topChildren` after `children` so that they stay on top,
    -- if I remember correctly
    div (children ++ topChildren)


viewSideMenu : C.Focus -> M.Model -> Html.Html Msg
viewSideMenu focus model =
  let

    topHeight = (model.dim.height * model.dim.heightProp) // 100
    pos = case focus of
      C.Top -> 0
      C.Bot -> topHeight

    sideWin = (Lens.get (M.workspaceLens focus) model).side
    menuElem onClick selected txt = Html.span
      [ Atts.class "noselect"
      , Events.onClick onClick
--       -- @tabindex required to make the div propagate the keyboard events
--       -- (see the `view` function)
--       , Atts.attribute "tabindex" "1"
      , Atts.style <|
        [ "cursor" :> "pointer"
        -- NOTE: inline-block hidden because we want the side menu to be
        -- relatively fixed:
        -- , "display" :> "inline-block"
        , "margin" :> px 5
        ] ++ if selected
             then ["font-weight" :> "bold"]
             else []
      ]
      -- [ Html.text txt ]
      [ txt ]

  in

    Html.div
      [ Atts.style
          [ "position" :> "fixed"
          , "background-color" :> "white" -- "#eee"
          -- , "width" :> "100%" -- <- hids the scrollbar! hence opacity
          , "opacity" :> "0.9"
          -- , "z-index" :> "1"
          , "top" :> px pos
          -- make the (focus-related) outline invisible
          , "outline" :> "0" ]
      -- @tabindex required to make the div propagate the keyboard events
      -- (see the `view` function)
      , Atts.attribute "tabindex" "1"
      ]
      [ menuElem (SideMenuEdit focus) (sideWin == M.SideEdit) (emphasize 0 "Edit")
      , menuElem (SideMenuContext focus) (sideWin == M.SideContext) (emphasize 0 "Context")
      , menuElem (SideMenuLog focus) (sideWin == M.SideLog) (plainText "Messages") ]


-- | The view of the side edit window -- the label.
viewSideEditLabel : C.Focus -> M.Model -> Html.Html Msg
viewSideEditLabel win model =
  let
    selected = (Lens.get (M.windowLens win) model).selMain
--     selected = case win of
--       C.Top -> model.top.selMain
--       C.Bot -> model.bot.selMain
    (condAtts, event) = case selected of
      Just nodeId ->
        ( [Atts.value (M.getLabel nodeId win model)]
        , ChangeLabel nodeId win )
      Nothing ->
        ( [Atts.disabled True, Atts.placeholder "<label>"]
        , \_ -> Msg.dummy )
    inp = Html.input
      ( [ Events.onInput event
        , blockKeyDownEvents
        , Atts.id <| case win of
            C.Top -> Cfg.editLabelName True
            C.Bot -> Cfg.editLabelName False
        ] ++ condAtts
      )
      []
  in
    inp


-- | The view of the side edit window -- the label.
viewSideEditLeaf
    : M.Model -> C.Focus -> C.NodeId -> M.InternalNode -> List (Html.Html Msg)
viewSideEditLeaf model focus nodeId node =
  let

    inpLabel = textGenericGen
               (SetNodeAttr nodeId focus)
               Msg.NodeLabelAttr
               "Label: "
               node.nodeVal
               (Just <| case focus of
                            C.Top -> Cfg.editLabelName True
                            C.Bot -> Cfg.editLabelName False)

    inpComment = textGenericGen
                 (SetNodeAttr nodeId focus)
                 Msg.NodeCommentAttr
                 "Comment: "
                 node.nodeComment
                 Nothing
    -- inpCardinality = textGeneric Anno.CardinalityAttr "Cardinality: " ev.evCardinality

  in
      [ Html.table [] <|
            [ inpLabel
            , inpComment
            ]
      ]


-- | The view of the side edit window -- the label.
viewSideEditInternal
    : M.Model
    -> C.Focus
    -> C.NodeId
    -> M.NodeTyp
    -> M.InternalNode
    -> List (Html.Html Msg)
viewSideEditInternal model focus nodeId nodeTyp node =
  let

    preTerminals = model.annoConfig.preTerminals
    nonTerminals = model.annoConfig.nonTerminals
    allTerminals = nonTerminals ++ preTerminals
    labelSet = if nodeTyp == M.PosNode
               then preTerminals
               else if nodeTyp == M.Phrasal
                    then nonTerminals
                    else allTerminals
    inpLabel = inputGenericConstrainedGen
               (SetNodeAttr nodeId focus)
               "Label"
               node.nodeVal
               labelSet
               Msg.NodeLabelAttr
               (Just <| case focus of
                            C.Top -> Cfg.editLabelName True
                            C.Bot -> Cfg.editLabelName False)

    inpComment = textGenericGen
                 (SetNodeAttr nodeId focus)
                 Msg.NodeCommentAttr
                 "Comment: "
                 node.nodeComment
                 Nothing
    -- inpCardinality = textGeneric Anno.CardinalityAttr "Cardinality: " ev.evCardinality

  in
      [ Html.table [] <|
            [ inpLabel
            , inpComment
            ]
      ]


-- viewSideEvent : C.Focus -> C.NodeId -> Anno.Event -> List (Html.Html Msg)
-- viewSideEvent focus nodeId (Anno.Event ev) =
--   let
--
--     inputGeneric = inputGenericGen (SetEventAttr nodeId focus)
--     inpClass = inputGeneric "Class" ev.evClass Anno.eventClassStr Anno.ClassAttr
--     inpType = inputGeneric "Type" ev.evType Anno.eventTypeStr Anno.TypeAttr
--     inpInq = inputGeneric "Inquisit" ev.evInquisit Anno.eventInquisitStr Anno.InquisitAttr
--     inpTime =
--         inputGeneric "Time" ev.evTime
--             (Anno.nullable Anno.eventTimeStr)
--             Anno.TimeAttr
--     inpAspect =
--         inputGeneric "Aspect" ev.evAspect
--             (Anno.nullable Anno.eventAspectStr)
--             Anno.AspectAttr
--     inpPolar = inputGeneric "Polarity" ev.evPolarity Anno.eventPolarityStr Anno.PolarityAttr
--     inpMood =
--         inputGeneric "Mood" ev.evMood
--             (Anno.nullable Anno.eventMoodStr)
--             Anno.MoodAttr
--     inpModality =
--         inputGeneric "Modality" ev.evModality
--             (Anno.nullable Anno.eventModalityStr)
--             Anno.ModalityAttr
--     inpMod =
--         inputGeneric "Mod" ev.evMod
--             (Anno.nullable Anno.eventModStr)
--             Anno.ModAttr
--
--     textGeneric = textGenericGen (SetEventAttr nodeId focus)
--     inpCardinality = textGeneric Anno.CardinalityAttr "Cardinality: " ev.evCardinality Nothing
--     inpPred = textGeneric Anno.PredAttr "Pred: " ev.evPred Nothing
--     -- inpComment = textGeneric Anno.CommentAttr "Comment: " ev.evComment
--
--   in
--       [ Html.hr [] []
--       , Html.text "Event:"
--       , Html.table []
--             [ inpClass, inpType, inpInq, inpTime, inpAspect , inpPolar, inpMood
--             , inpModality, inpCardinality, inpMod, inpPred ] -- , inpComment ]
--       ]
--
--
-- viewSideSignal : C.Focus -> C.NodeId -> Anno.Signal -> List (Html.Html Msg)
-- viewSideSignal focus nodeId (Anno.Signal x) =
--   let
--
--     inputGeneric = inputGenericGen (SetSignalAttr nodeId focus)
--
--     inpType = inputGeneric "Type" x.siType Anno.signalTypeStr Anno.SiTypeAttr
--
--   in
--       [ Html.hr [] []
--       , Html.text "Signal:"
--       , Html.table []
--             [ inpType ]
--       ]
--
--
-- viewSideTimex : M.Model -> C.Focus -> C.NodeId -> M.InternalNode -> Anno.Timex -> List (Html.Html Msg)
-- viewSideTimex model focus nodeId node (Anno.Timex ti) =
--   let
--
--     inputGeneric = inputGenericGen (SetTimexAttr nodeId focus)
--     inpCalendar = inputGeneric "Calendar" ti.tiCalendar Anno.timexCalendarStr Anno.TiCalendarAttr
--     inpFunctionInDocument =
--         inputGeneric "Function" ti.tiFunctionInDocument
--             (Anno.nullable Anno.timexFunctionInDocumentStr)
--             Anno.TiFunctionInDocumentAttr
--     inpType = inputGeneric "Type" ti.tiType Anno.timexTypeStr Anno.TiTypeAttr
--     inpTemporalFunction =
--         inputGeneric "Temporal Fun" ti.tiTemporalFunction
--             (Anno.nullable Anno.timexTemporalFunctionStr)
--             Anno.TiTemporalFunctionAttr
--     inpMod =
--         inputGeneric "Mod" ti.tiMod
--             (Anno.nullable Anno.timexModStr)
--             Anno.TiModAttr
--
--     inputTimex = inputTimexGen model focus (SetTimexAttr nodeId focus)
--     inpAnchor = inputTimex "Anchor" ti.tiAnchor Anno.TiAnchorAttr
--     inpBeginPoint = inputTimex "Begin" ti.tiBeginPoint Anno.TiBeginPointAttr
--     inpEndPoint = inputTimex "End" ti.tiEndPoint Anno.TiEndPointAttr
--
--     textGeneric = textGenericGen (SetTimexAttr nodeId focus)
--     inpPred = textGeneric Anno.TiPredAttr "Pred: " ti.tiPred Nothing
--     inpLingValue = textGeneric Anno.TiLingValueAttr "LingValue: " ti.tiLingValue Nothing
--     inpValue = textGeneric Anno.TiValueAttr "Value: " ti.tiValue Nothing
--
--     mayTextGeneric = mayTextGenericGen (SetTimexAttr nodeId focus)
--     inpQuant = mayTextGeneric Anno.TiQuantAttr "Quant:" ti.tiQuant
--     inpFreq = mayTextGeneric  Anno.TiFreqAttr "Freq:" ti.tiFreq
--
--     typeDependent =
--         case ti.tiType of
--             Anno.Duration -> [inpBeginPoint, inpEndPoint]
--             Anno.Set -> [inpQuant, inpFreq]
--             _ -> []
--
--   in
--       [ Html.hr [] []
--       -- , Html.h3 [] [Html.text "Timex:"]
--       , Html.text "Timex:"
--       , Html.table [] <|
--             [ inpCalendar, inpFunctionInDocument, inpPred, inpType
--             , inpTemporalFunction, inpLingValue, inpValue, inpMod
--             , inpAnchor ] ++ typeDependent
--       ]


viewSideEntity
     : M.Model
    -> C.Focus
    -> C.NodeId
    -> M.InternalNode
    -> Anno.Entity
    -> List (Html.Html Msg)
viewSideEntity model focus nodeId node ent =
  let
    entCfg = AnnoCfg.entityConfig ent.name model.annoConfig

    setAnchor attrName create =
        if create
        then SetEntityAnchor nodeId focus attrName
        else SetEntityAttr nodeId focus attrName Nothing

    inputAttr attrName attrCfg =
      case attrCfg of
        AnnoCfg.Anchor ->
            inputAnchor (setAnchor attrName) attrName model
        AnnoCfg.Closed r ->
            inputClosed (SetEntityAttr nodeId focus attrName) attrName r
        AnnoCfg.Free r ->
            inputFree (SetEntityAttr nodeId focus attrName) attrName r

    inputType = inputTypeGen (SetEntityType nodeId focus) entCfg.typ ent.typ
    procAtts (attrName, attrCfg) =
        inputAttr attrName attrCfg (D.get attrName ent.attributes)
    inputCoreAtts = L.map procAtts entCfg.attributes
    inputDepAtts = L.map procAtts
      (Maybe.withDefault [] <| D.get ent.typ entCfg.attributesOnType)
  in
      [ Html.hr [] []
      , Html.text <| ent.name ++ ":"
      , Html.table [] <| inputType :: (inputCoreAtts ++ inputDepAtts)
      ]


-- | The view of a side window.
viewSideEdit : Bool -> C.Focus -> M.Model -> Html.Html Msg
viewSideEdit visible win model =
  let

    selected = (Lens.get (M.windowLens win) model).selMain
--     selected = case win of
--       C.Top -> model.top.selMain
--       C.Bot -> model.bot.selMain

    divMain = case selected of
      Nothing -> []
      Just nodeId -> case M.getNode nodeId win model of
        M.Leaf r -> [viewSideEditLabel win model]
        M.Node r ->
            let
                -- TEMP 28/11: seems OK
                partId = M.getReprId win (M.selectWin win model).tree model
                tree = M.getTree win partId model
                nodeTyp = Maybe.withDefault M.Internal <| M.getNodeTyp nodeId tree
            in
                viewSideEditInternal model win nodeId nodeTyp r

    divChildren = case selected of
      Nothing -> []
      Just nodeId -> case M.getNode nodeId win model of
        M.Leaf r -> []
        M.Node r -> case r.nodeTyp of
          Nothing -> []
          Just en -> viewSideEntity model win nodeId r en
--           Just (M.NodeTimex ti) -> viewSideTimex model win nodeId r ti
--           Just (M.NodeSignal si) -> viewSideSignal win nodeId si
--           Just (M.NodeEvent ev) -> viewSideEvent win nodeId ev

    div = Html.div
      [ Atts.style
        [ "position" :> "absolute"
        -- , "width" :> "50%"
        , "top" :> px Cfg.sideMenuHeight
        , "margin" :> px 5
        ]
      ]
      (divMain ++ divChildren)
    top = viewSideDiv visible win model [div]
  in
    top


---------------------------------------------------
-- Side context window
---------------------------------------------------


-- | The view of a side window.
viewSideContext
    : Bool           -- ^ Visible?
    -> C.Focus
    -> M.Model
    -> Html.Html Msg
viewSideContext visible foc model =

  let

    -- | The set of all speakers in the file.
    spkAll : S.Set String
    -- spkAll = Util.unions <| List.map getSpeakers <| model.file.turns
    spkAll = Util.unions
             <| List.map getSpeakers
             <| (Lens.get (M.fileLens foc) model).turns

    spkHeader =
        Html.thead []
            [Html.tr [] <| List.map viewSpkHead <| S.toList spkAll]

    viewSpkHead : String -> Html.Html Msg
    viewSpkHead spk =
        let color = backColor foc model in
        Html.th
            [Atts.style ["border-bottom" :> ("2px solid " ++ color)]]
            [Html.text spk]

    viewTurnAlt : D.Dict String C.TreeId -> Html.Html Msg
    viewTurnAlt turnDict =
        let
            viewSpk spk =
                case D.get spk turnDict of
                    Nothing -> Html.td [] []
                    Just treeId ->
                        Html.td []
                            [viewSentAlt foc treeId spk model]
        in
            Html.tr []
            <| List.map viewSpk
            <| S.toList spkAll



    divAlt = viewSideDiv visible foc model
      [ Html.table
          [Atts.style
             [ "position" :> "absolute"
             , "top" :> px Cfg.sideMenuHeight
             , "border-collapse" :> "collapse"
               -- ^ so that bottom border is continuous
             ]
          ] <|
          spkHeader ::
          -- (List.map viewTurnAlt <| List.map inverseTurn <| model.file.turns)
          ( List.map viewTurnAlt
                <| List.map inverseTurn
                <| (Lens.get (M.fileLens foc) model).turns
          )
      ]

  in

    divAlt


-- | Tranform a turn so that it becomes a map from speakers to the corresponding
-- trees.  It assumes that a speaker speeks at most once in a given turn.
inverseTurn : M.Turn -> D.Dict String C.TreeId
inverseTurn turn =
    let
        inverse (treeId, mayWho) =
            let
                default =
                    case turn.speaker of
                        [x] -> x
                        _ -> "?"
                spk = mayWho
                    |> Maybe.andThen (\id -> Util.at (id - 1) turn.speaker)
                    |> Maybe.withDefault default
            in
                (spk, C.TreeId treeId)
    in
        D.fromList
            <| L.map inverse
            <| D.toList turn.trees


-- | Retrieve the set of speakers in the given turn.
getSpeakers : M.Turn -> S.Set String
getSpeakers = S.fromList << D.keys << inverseTurn


viewSentAlt
  : C.Focus   -- ^ Where is the focus on
  -> C.TreeId -- ^ The tree ID (the representative) ...
  -> String   -- ^ The speaker
  -> M.Model
  -> Html.Html Msg
viewSentAlt foc treeId spk model =
  let
    (sent, firstTokId) = M.getSubSent foc treeId model
    partId = M.getReprId foc treeId model
    tree = M.getTree foc partId model
    partSelected = M.getReprId foc (M.selectWin foc model).tree model
    isSelected = partId == partSelected
    paraAtts = if isSelected
      then [ Atts.style ["font-weight" :> "bold"] ]
      else []
    divAtts = if isSelected
      then [ Atts.id <| Cfg.selectSentName <|
                 case foc of
                     C.Top -> True
                     C.Bot -> False ]
      else []
    visible = M.visiblePositions tree
    isVisible tokID = S.member tokID visible
    para =
        Html.span paraAtts <|
            L.map (\(tokID, tok) -> viewToken foc partId (isVisible tokID) tokID tok) <|
                   L.map2 (,)
                       (L.map (\x -> x + firstTokId) <| L.range 0 (L.length sent - 1))
                       sent
    div =
      Html.div (
        [ Atts.class "noselect"
        , Events.onClick (SelectTree foc partId)
        , Atts.style
            [ "cursor" :> "pointer"
            -- make the (focus-related) outline invisible
            , "outline" :> "0" ]
        -- @tabindex required to make the div propagate the keyboard events
        -- (see the `view` function)
        , Atts.attribute "tabindex" "1"
        ] ++ divAtts )
        [para]
  in
    div


-- | View token.
viewToken
    : C.Focus     -- ^ Model focus
    -> C.PartId   -- ^ Partition ID
    -> Bool       -- ^ Is it visible?
    -> Int        -- ^ Token ID
    -> M.Token
    -> Html.Html Msg
viewToken focus partId isVisible tokID tok =
    let
        orth = (if tok.afterSpace then " " else "") ++ tok.orth
        color = if isVisible then "black" else "grey"
        styleAtts = [Atts.style ["cursor" :> "pointer", "color" :> color]]
        eventAtts =
            [ Events.onClick (SelectToken focus partId tokID)
            ]
        atts = styleAtts ++ eventAtts
    in
        Html.span
            atts
            [ Html.text orth ]


---------------------------------------------------
-- Side log (messages)
---------------------------------------------------


-- | The view of a side window.
viewSideLog : Bool -> C.Focus -> M.Model -> Html.Html Msg
viewSideLog visible foc model =
  let
    treeSelected = (M.selectWin foc model).tree
    div = viewSideDiv visible foc model
      [ Html.ul
          [Atts.style
             [ "position" :> "absolute"
             , "top" :> px Cfg.sideMenuHeight ]
          ]
          (List.map
             (\msg -> viewMessage foc msg)
             model.messages
          )
      ]
  in
    div


viewMessage
  : C.Focus -- ^ Where is the focus on
  -> String -- ^ Message
  -> Html.Html Msg
viewMessage foc msg =
  let
    para = Html.p [] [Html.text msg]
    li =  Html.li [] <| Util.single <|
      Html.div
        [Atts.class "noselect"]
        [para]
  in
    li


---------------------------------------------------
-- Links
---------------------------------------------------


-- | Only if not in the adjudication mode.
viewLinks
    : M.Model
    -> List (Html.Html Msg)
viewLinks model =
    let
        fileTop = Lens.get (M.top => M.fileId) model
        fileBot = Lens.get (M.bot => M.fileId) model
        linkSet = Lens.get (M.fileLens C.Top => M.linkSet) model
    in
        if fileTop == fileBot
        then L.concatMap
            (viewLink model)
            (D.toList linkSet)
        else []
--     case model.cmpFile of
--         Just _  -> []
--         Nothing ->
--             L.concatMap
--                 (viewLink model)
--                 (D.toList model.mainFile.linkSet)


-- | View the link only if not in the adjudication mode.
viewLink
   : M.Model
  -> (C.Link, M.LinkData)
  -> List (Html.Html Msg)
viewLink model link =
    let
        fileTop = Lens.get (M.top => M.fileId) model
        fileBot = Lens.get (M.bot => M.fileId) model
    in
        if fileTop == fileBot
        then viewLink_ model link
        else []


-- | Internal view link, which does not check that we are not in the
-- adjudication mode.
viewLink_
   : M.Model
  -> (C.Link, M.LinkData)
  -> List (Html.Html Msg)
viewLink_ model ((from, to), linkData) =
  let
    -- top = model.top
    top = Lens.get (M.windowLens C.Top) model
    -- bot = model.bot
    bot = Lens.get (M.windowLens C.Bot) model
    dim = model.dim

    -- mainWidth = (dim.width * (100 - Cfg.sideSpace)) // 100
    mainWidth = (dim.width * dim.widthProp) // 100
    topHeight = (dim.height * dim.heightProp) // 100
    botHeight = dim.height - topHeight

    trimTop pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= topHeight - Cfg.dmzSize
      then Just pos
      else Nothing

    trimBot pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= botHeight - Cfg.dmzSize
      then Just {pos | y = pos.y + topHeight}
      else Nothing

    -- Safe because we know we are not in the adjudication mode.
    getReprId = M.getReprId C.Top

  in

    if
      getReprId top.tree model == first from &&
      getReprId bot.tree model == first to
    then
      viewLinkDir model (top, bot) (trimTop, trimBot) (from, to, linkData.signalAddr)
    else if
      getReprId bot.tree model == first from &&
      getReprId top.tree model == first to &&
      getReprId top.tree model /= getReprId bot.tree model
    then
      -- viewLinkDir model (top, bot) (trimTop, trimBot) (from, to, linkData.signalAddr)
      viewLinkDir model (bot, top) (trimBot, trimTop) (from, to, linkData.signalAddr)
    else
      []


viewLinkDir
   : M.Model
  -> (M.Window, M.Window)
  -> (Position -> Maybe Position, Position -> Maybe Position)
     -- ^ Shifting functions, which calculate the absolute positions for the
     -- corresponding (top/bottom) workspaces (return `Nothing` when they go
     -- beyond their workspaces)
  -> (C.Addr, C.Addr, Maybe C.Addr)
     -- ^ (from, to, maybe signal) addresses
  -> List (Html.Html Msg)
viewLinkDir model (top, bot) (shiftTop, shiftBot) (from, to, signalMay) =
  let

    nodePos1 nodeId pos tree = Tree.nodePos nodeId
      <| Tree.positionTree pos
      <| R.withWidth Tree.stdWidth Cfg.stdMargin tree
    posIn addr foc win shift = Maybe.andThen shift <| nodePos1
      (second addr)
      (M.getPosition foc model)
      (M.getTree foc (M.getReprId foc win.tree model) model)

    fromPos =
      if first from == M.getReprId C.Top top.tree model
      then posIn from C.Top top shiftTop
      else posIn from C.Bot bot shiftBot
    toPos = -- posIn to bot shiftBot
      if first to == M.getReprId C.Top bot.tree model
      then posIn to C.Bot bot shiftBot
      else posIn to C.Top top shiftTop
    signPos = case signalMay of
      Nothing -> Nothing
      Just addr ->
        if first addr == M.getReprId C.Top bot.tree model
        then posIn addr C.Bot bot shiftBot
        else posIn addr C.Top top shiftTop

    defLineCfg = Tree.defLineCfg
    lineCfg = { defLineCfg
      | strokeDasharray = Just Cfg.linkDasharray
      , strokeWidth = Cfg.linkWidth
      , opacity = Cfg.linkOpacity }

    circleCfg = { defCircleCfg
      | opacity = Cfg.linkOpacity
      , width = Cfg.linkHeadSize
      , height = Cfg.linkHeadSize }

  in

    case (fromPos, toPos) of
      (Just p, Just q) ->
        let
          trimLine = trimBeg Cfg.linkTailDist << trimEnd Cfg.linkHeadDist
          midCirc = {x = (p.x + q.x) // 2, y = (p.y + q.y) // 2}
          endCirc = (trimEnd Cfg.linkHeadDist2 {beg=p, end=q}).end
          lin1 = trimLine <| {beg=p, end=midCirc}
          lin2 = trimLine <| {beg=midCirc, end=endCirc}
          lin3May = case signPos of
            Nothing -> Nothing
            Just z -> Just <| trimLine <| {beg=midCirc, end=z}
        in
          [ Tree.viewLine lineCfg lin1.beg lin1.end
          , Tree.viewLine lineCfg lin2.beg lin2.end
          , drawCircle circleCfg endCirc
          , drawLinkCircle model (from, to) midCirc ]
          ++ case lin3May of
               Nothing -> []
               Just lin3 -> [Tree.viewLine lineCfg lin3.beg lin3.end]
      _ -> []


-- | Draw the circle which represents the relation.
drawLinkCircle
    : M.Model
    -> C.Link
    -> Position
    -> Html.Html Msg
drawLinkCircle model link at =
  let
    cfg0 = { defCircleCfg
      | opacity = Cfg.linkCircleOpacity
      , color = Cfg.linkCircleColor
      , height = Cfg.linkCircleRadius
      , width = Cfg.linkCircleRadius }
    cfg = if model.selLink == Just link
      then {cfg0 | color = Cfg.linkCircleSelectColor}
      else cfg0
  in
    Html.div
      [ circleStyle cfg at
      , Events.onClick (SelectLink link)

      -- @tabindex required to make the div register keyboard events
      -- , Atts.attribute "tabindex" "1"
      ]
      []


---------------------------------------------------
-- Circles
---------------------------------------------------


type alias CircleCfg =
  { color : String
  , opacity : String
  , height : Int
  , width : Int
  }


defCircleCfg : CircleCfg
defCircleCfg =
  { color = "black"
  , opacity = "1"
  , height = 10
  , width = 10
  }


drawCircle : CircleCfg -> Position -> Html.Html msg
drawCircle cfg at = Html.div [circleStyle cfg at] []


circleStyle : CircleCfg -> Position -> Html.Attribute msg
circleStyle cfg at = Atts.style
  [ "background-color" :> cfg.color
  , "opacity" :> cfg.opacity
  , "width" :> px cfg.width
  , "height" :> px cfg.height
  , "border-radius" :> "50%"
  , "position" :> "absolute"
  , "left" :> px (at.x - cfg.width // 2)
  , "top" :> px (at.y - cfg.height // 2)
  ]


---------------------------------------------------
-- Events
---------------------------------------------------


nodeMouseDown : C.Focus -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win <| Lens.get M.nodeId x)


backMouseDown : C.Focus -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)


-- backDoubleClick : C.Focus -> Html.Attribute Msg
-- backDoubleClick win =
--   Events.onDoubleClick (Focus win)


winOnFocus : C.Focus -> Html.Attribute Msg
winOnFocus win =
  Events.onFocus (Focus win)


-- | Decode the height of a given element.
offsetHeight : Decode.Decoder Int
offsetHeight =
  Decode.at ["target", "offsetHeight"] Decode.int


---------------------------------------------------
-- Keyboard Events
---------------------------------------------------


globalKeyDown : M.Model -> Html.Attribute Msg
globalKeyDown model =
    case model.command of
        Nothing -> mainKeyDown model.annoConfig model.ctrl
        Just _ -> cmdKeyDown


cmdKeyDown : Html.Attribute Msg
cmdKeyDown =
  let
    tag code = case code of
      8 -> CommandBackspace
      9 -> CommandComplete
      13 -> CommandEnter
      27 -> CommandEscape
      _ -> CommandChar (Char.toLower <| Char.fromCode code)
      -- _ -> CommandString (toString code ++ " ")
      -- _ -> Debug.crash <| toString (code, Char.fromCode code, Char.toLower <| Char.fromCode code)
  in
    onKeyDown tag


mainKeyDown
  :  AnnoCfg.Config -- ^ CTRL down?
  -> Bool -- ^ CTRL down?
  -> Html.Attribute Msg
mainKeyDown annoCfg ctrl =
  let
    tag code = case code of
      -- "PgUp" and "PgDown"
      33 -> Previous
      34 -> Next

--       -- "d"
--       68 ->
--         if ctrl
--         then DeleteTree
--         else Delete
--       -- -- "Del"
--       -- 46 -> Delete

--       -- "a"
--       65 -> Add

      -- "c"
      67 -> ShowContext

--       -- "s"
--       83 -> MkSignal

--       -- "t"
--       -- 84 -> ChangeType
--       84 -> MkTimex

--       -- "v"
--       86 -> MkEvent

      -- -- "+" and "-"
      -- 107 -> Increase True
      -- 109 -> Increase False
      -- "up" and "down" -- horizontal axe
      40 -> Increase True True
      38 -> Increase True False
      -- "left" and "right" -- vertical axe
      37 ->
        if ctrl
        then Swap True
        else Increase False False
      39 ->
        if ctrl
        then Swap False
        else Increase False True

      -- "e"
      69 -> EditLabel

      -- "TAB"
      9 -> SwapFile

      -- "ctrl"
      17 -> CtrlDown

      -- "c"
      -- 67 -> Connect

--       -- "p"
--       80 -> ParseSent Server.Stanford

      -- "r"
      82 -> Attach

      -- "z"
      90 ->
        if ctrl
        then Undo
        else Redo

      -- "space"
      32 -> CommandStart

--       -- "enter"
--       13 -> CommandEnter

      -- "escape"
      -- 27 -> Popup (Popup.Files Nothing) Nothing
      27 -> Quit

      _  ->
          case Cmd.msgFromKeyCode annoCfg ctrl code of
              Nothing -> Msg.dummy
              Just msg -> msg
  in
    onKeyDown tag


globalKeyUp : Html.Attribute Msg
globalKeyUp =
  let
    tag code = case code of

      -- "ctrl"
      17 -> CtrlUp

      _  -> Msg.dummy
  in
    onKeyUp tag


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
  Events.on "keyup" (Decode.map tagger Events.keyCode)


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  -- Events.on "keydown" (Decode.map tagger Events.keyCode)
  Events.onWithOptions
    "keydown"
    (let default = Events.defaultOptions
     in {default | stopPropagation=True, preventDefault=True})
    (Decode.map tagger Events.keyCode)


-- | Block all key events with the exception of CTRL!
blockKeyDownEvents : Html.Attribute Msg
blockKeyDownEvents =
    let
        options =
            (let default = Events.defaultOptions
             in {default | stopPropagation=True})
        filterKey code =
            if code == 17 then -- CTRL
                Decode.fail "won't be blocked"
            else
                Decode.succeed Msg.dummy
        decoder =
            Events.keyCode
                |> Decode.andThen filterKey
    in
        Events.onWithOptions "keydown" options decoder


-- | Keydown events related to popups.
popupKeyDownEvents : (Int -> msg) -> Html.Attribute msg
popupKeyDownEvents tagger =
    let
        options =
            (let default = Events.defaultOptions
             in {default | stopPropagation=True})
        filterKey code =
            if code == 17 then -- CTRL
                Decode.fail "won't be blocked"
            else
                -- Decode.succeed Msg.dummy
                Decode.succeed (tagger code)
        decoder =
            Events.keyCode
                |> Decode.andThen filterKey
    in
        Events.onWithOptions "keydown" options decoder


---------------------------------------------------
-- Vectors
---------------------------------------------------


type alias Vect = {beg : Position, end : Position}


-- | Length of a vector.
length : Vect -> Float
length {beg, end} =
  let
    x = end.x - beg.x
    y = end.y - beg.y
  in
    sqrt <| toFloat (x*x + y*y)


-- | Inverse a vector.
inverse : Vect -> Vect
inverse {beg, end} = {beg=end, end=beg}


-- | Shorten the end of a given vector by a given length.
trimEnd : Int -> Vect -> Vect
trimEnd trim ({beg, end} as v) =
  let
    trimProp = toFloat trim / length v
    restProp = 1.0 - trimProp
    x = beg.x + round (toFloat (end.x - beg.x) * restProp)
    y = beg.y + round (toFloat (end.y - beg.y) * restProp)
  in
    {beg=beg, end={x=x, y=y}}


-- | Shorten the beinning of a given vector by a given length.
trimBeg : Int -> Vect -> Vect
trimBeg trim v = inverse <| trimEnd trim <| inverse v


---------------------------------------------------
-- Utils
---------------------------------------------------


(:>) : a -> b -> (a, b)
(:>) = (,)


px : Int -> String
px number =
  toString number ++ "px"


ps : Int -> String
ps number =
  toString number ++ "%"


---------------------------------------------------
-- Viewing-related utility functions
---------------------------------------------------


-- | Doubly generic textual input field.
-- `mayId` represents the optional ID of the field.
textGenericGen setAttr attr text value mayId =
  let
    setMsg = setAttr << attr
    idAtts = case mayId of
                 Nothing -> []
                 Just x -> [Atts.id x]
  in
    Html.tr []
      [ Html.td
          [ Atts.class "noselect"
          , Atts.align "right"
          ]
          [Html.text text]
      , Html.td []
          [Html.input
               ( [ Events.onInput setMsg
                 -- , Atts.rows 3
                 , Atts.value value
                 , blockKeyDownEvents
                 ] ++ idAtts )
               []
          ]
      ]



-- | Doubly generic (maybe) textual input field.
mayTextGenericGen setAttr attr text mayValue =
  let
    setMsg = setAttr << attr << Just
    value = case mayValue of
      Nothing -> ""
      Just x  -> x
  in
    Html.tr []
      [ Html.td
          [ Atts.class "noselect"
          , Atts.align "right"
          ]
          [Html.text text]
      , Html.td []
          [Html.input
               [ Events.onInput setMsg
               -- , Atts.rows 3
               , Atts.value value
               , blockKeyDownEvents ]
               []
          ]
      ]


-- | Type input field.
inputTypeGen
    : (String -> Msg)
    -- ^ Message to send when a type value is selected
    -> AnnoCfg.EntityType
    -- ^ The chosen type value
    -> String
    -- ^ The chosen type value
    -> Html.Html Msg
inputTypeGen setMsg typCfg typValue =
  let
    labelTD =
        Html.td
            [ Atts.class "noselect"
            , Atts.align "right" ]
            [Html.text "Type: "]
    option val = Html.option
      [ Atts.value val
      , Atts.selected (val == typValue) ]
      [ Html.text val ]
  in
    Html.tr []
      [ labelTD
      , Html.td []
          [Html.select
               [ Events.on "change" (Decode.map setMsg Events.targetValue)
               , blockKeyDownEvents ]
               (List.map option typCfg.among)
          ]
      ]


-- | Anchor input field...
inputAnchor
    : (Bool -> Msg)
    -- ^ Message to send on Create/Remove
    -> String
    -- ^ Anchor name
    -> M.Model
    -- ^ The model
    -> Maybe Anno.Attr
    -- ^ The chosen attribute value
    -> Html.Html Msg
inputAnchor setMsg anchorName model attrValue =
  let
    labelTD =
        Html.td
            [ Atts.class "noselect"
            , Atts.align "right" ]
            [Html.text (anchorName ++ ": ")]
    value =
      case attrValue of
        Just (Anno.Anchor x) -> Just x
        _ -> Nothing
    html =
      case value of
          Nothing ->
              [ Html.button
                    [ Events.onClick (setMsg True)
                    , Atts.title "Link (i) with the additionally selected node in focus, or (ii) with the main selected node in the other window" ]
                    [Html.text "Create"]
              ]
          Just addr ->
              let
                  subTree = M.subTreeAt addr model.focus model
                  rootLabel = Lens.get M.nodeVal <| R.label subTree
                  words = String.join " " <| L.map (\r -> r.nodeVal) <| M.getWords subTree
                  string = rootLabel ++ " (\"" ++ words ++ "\")"
--                   positions = L.map (\r -> r.leafPos) <| M.getWords subTree
--                   beg = case L.minimum positions of
--                             Nothing -> ""
--                             Just x  -> toString x
--                   end = case L.maximum positions of
--                             Nothing -> ""
--                             Just x  -> toString x
--                   string = rootLabel ++ " (\"" ++ beg ++ ", " ++ end ++ "\")"
              in
                  [ Html.span
                        [ Atts.class "noselect"
                        , Atts.style ["cursor" :> "pointer"]
                        , Atts.title "Goto"
                        , Events.onClick <|
                            Many
                            [ SelectTree model.focus (Tuple.first addr)
                            , Select model.focus (Tuple.second addr) ]
                        ]
                        [ Html.text string ]
                  , Html.button
                        [ Atts.style ["margin-left" :> px 5]
                        , Events.onClick (setMsg False)]
                        [ Html.text "Remove" ]
                  ]
  in
    Html.tr []
      [ labelTD
      , Html.td [] html
      ]


-- | Closed input field.
inputClosed
    : (Maybe Anno.Attr -> Msg)
    -- ^ Message to send when an attribute is selected
    -> String
    -- ^ Attribute name
    -> AnnoCfg.ClosedRec
    -- ^ The configuration corresponding to the attribute
    -> Maybe Anno.Attr
    -- ^ The chosen attribute value
    -> Html.Html Msg
inputClosed setAttr label r attrValue =
  let
    labelTD =
        Html.td
            [ Atts.class "noselect"
            , Atts.align "right" ]
            [Html.text (label ++ ": ")]
  in
    let
      setMsg newVal =
        case newVal of
          "--" -> setAttr Nothing
          _ -> setAttr (Just <| Anno.Attr newVal)
      among r =
        if r.required
        then L.map Just r.among
        else Nothing :: L.map Just r.among
      value =
        case attrValue of
          Just (Anno.Attr x) -> Just x
          _ -> Nothing
      val2str val =
        case val of
          Nothing -> "--"
          Just x  -> x
      option val = Html.option
        [ Atts.value (val2str val)
        , Atts.selected (val == value) ]
        [ Html.text (val2str val) ]
    in
      Html.tr []
        [ labelTD
        , Html.td []
            [Html.select
                 [ Events.on "change" (Decode.map setMsg Events.targetValue)
                 , blockKeyDownEvents ]
                 (List.map option (among r))
            ]
        ]


-- | Free input field.
inputFree
    : (Maybe Anno.Attr -> Msg)
    -- ^ Message to send when an attribute is selected
    -> String
    -- ^ Attribute name
    -> AnnoCfg.FreeRec
    -- ^ The configuration corresponding to the attribute
    -> Maybe Anno.Attr
    -- ^ The chosen attribute value
    -> Html.Html Msg
inputFree setAttr label _ attrValue =
  let
    labelTD =
        Html.td
            [ Atts.class "noselect"
            , Atts.align "right" ]
            [Html.text (label ++ ": ")]
  in
    let
      setMsg newVal =
        case newVal of
          "" -> setAttr Nothing
          _ -> setAttr (Just <| Anno.Attr newVal)
      value =
        case attrValue of
          Just (Anno.Attr x) -> x
          _ -> ""
    in
      Html.tr []
        [ labelTD
        , Html.td []
            [Html.input
                 [ Events.onInput setMsg
                 , Atts.value value
                 , blockKeyDownEvents ]
                 []
            ]
        ]


-- -- | Doubly generic input field...
-- inputGenericGen
--     : (Maybe Anno.Attr -> Msg)
--     -- ^ Message to send when an attribute is selected
--     -> String
--     -- ^ Attribute name
--     -> AnnoCfg.Attr
--     -- ^ The configuration corresponding to the attribute
--     -> Maybe Anno.Attr
--     -- ^ The chosen attribute value
--     -> Html.Html Msg
-- inputGenericGen setAttr label attrCfg attrValue =
--   let
--     labelTD =
--         Html.td
--             [ Atts.class "noselect"
--             , Atts.align "right" ]
--             [Html.text (label ++ ": ")]
--   in
--     case attrCfg of
--         AnnoCfg.Anchor -> Debug.crash "asdf"
--         AnnoCfg.Closed r ->
--             let
--               setMsg newVal =
--                 case newVal of
--                   "--" -> setAttr Nothing
--                   _ -> setAttr (Just <| Anno.Attr newVal)
--               among r =
--                 if r.required
--                 then L.map Just r.among
--                 else Nothing :: L.map Just r.among
--               value =
--                 case attrValue of
--                   Just (Anno.Attr x) -> Just x
--                   _ -> Nothing
--               val2str val =
--                 case val of
--                   Nothing -> "--"
--                   Just x  -> x
--               option val = Html.option
--                 [ Atts.value (val2str val)
--                 , Atts.selected (val == value) ]
--                 [ Html.text (val2str val) ]
--             in
--               Html.tr []
--                 [ labelTD
--                 , Html.td []
--                     [Html.select
--                          [ Events.on "change" (Decode.map setMsg Events.targetValue)
--                          , blockKeyDownEvents ]
--                          (List.map option (among r))
--                     ]
--                 ]
--         AnnoCfg.Free _ ->
--             let
--               setMsg newVal =
--                 case newVal of
--                   "" -> setAttr Nothing
--                   _ -> setAttr (Just <| Anno.Attr newVal)
--               value =
--                 case attrValue of
--                   Just (Anno.Attr x) -> x
--                   _ -> ""
--             in
--               Html.tr []
--                 [ labelTD
--                 , Html.td []
--                     [Html.input
--                          [ Events.onInput setMsg
--                          , Atts.value value
--                          , blockKeyDownEvents ]
--                          []
--                     ]
--                 ]


-- -- | Doubly generic list input field.
-- inputGenericGen setAttr label value valList attr =
--   let
--     setMsg str = setAttr (attr <| Anno.valueFromStr valList str)
--     option evVal (str, val) = Html.option
--       [ Atts.value str
--       , Atts.selected (val == evVal) ]
--       [ Html.text str ]
--   in
--     Html.tr []
--       [ Html.td
--           [ Atts.class "noselect"
--           , Atts.align "right"
--           ]
--           [Html.text (label ++ ": ")]
--       , Html.td []
--           [Html.select
--                [ Events.on "change" (Decode.map setMsg Events.targetValue)
--                , blockKeyDownEvents ]
--                (List.map (option value) valList)
--           ]
--       ]


-- | Doubly generic list input field.
inputGenericConstrainedGen setAttr label value valList attr mayId =
  let
    setMsg str = setAttr (attr str)
    option evVal val = Html.option
      [ Atts.value val
      , Atts.selected (val == evVal) ]
      [ Html.text val ]
  in
    Html.tr []
      [ Html.td
          [ Atts.class "noselect"
          , Atts.align "right"
          ]
          [Html.text (label ++ ": ")]
      , Html.td []
          [Html.select
             ( [ Events.on "change" (Decode.map setMsg Events.targetValue)
               , blockKeyDownEvents
               ] ++ case mayId of
                        Nothing -> []
                        Just x -> [Atts.id x]
             )
               (List.map (option value) valList)
          ]
      ]


-- -- | Generic TIMEX input field.
-- inputTimexGen
--     : M.Model
--     -> C.Focus
--     -> (Anno.TimexAttr -> Msg)
--     -> String
--     -> Maybe C.Addr
--     -> (Bool -> Anno.TimexAttr)
--     -> Html.Html Msg
-- inputTimexGen model focus setAttr label value attr =
--   let
--     setMsg = setAttr (attr True)
--     remMsg = setAttr (attr False)
--     html =
--         case value of
--             Nothing ->
--                 [ Html.button
--                       [ Events.onClick setMsg
--                       , Atts.title "Link (i) with the additionally selected node in focus, or (ii) with the main selected node in the other window" ]
--                       [Html.text "Create"]
--                 ]
--             Just addr ->
--                 let
--                     subTree = M.subTreeAt addr focus model
--                     rootLabel = Lens.get M.nodeVal <| R.label subTree
--                     words = String.join " " <| L.map (\r -> r.nodeVal) <| M.getWords subTree
--                     string = rootLabel ++ " (\"" ++ words ++ "\")"
-- --                     positions = L.map (\r -> r.leafPos) <| M.getWords subTree
-- --                     beg = case L.minimum positions of
-- --                               Nothing -> ""
-- --                               Just x  -> toString x
-- --                     end = case L.maximum positions of
-- --                               Nothing -> ""
-- --                               Just x  -> toString x
-- --                     string = rootLabel ++ " (\"" ++ beg ++ ", " ++ end ++ "\")"
--                 in
--                     [ Html.span
--                           [ Atts.class "noselect"
--                           , Atts.style ["cursor" :> "pointer"]
--                           , Atts.title "Goto"
--                           , Events.onClick <|
--                               Many
--                               [ SelectTree model.focus (Tuple.first addr)
--                               , Select model.focus (Tuple.second addr) ]
--                           ]
--                           [ Html.text string ]
--                     , Html.button
--                           [ Atts.style ["margin-left" :> px 5]
--                           , Events.onClick remMsg]
--                           [ Html.text "Remove" ]
--                     ]
--   in
--     Html.tr []
--       [ Html.td
--           [ Atts.class "noselect"
--           , Atts.align "right"
--           ]
--           [Html.text (label ++ ": ")]
--       , Html.td [] html
--       ]


---------------------------------------------------
-- Showing text
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
