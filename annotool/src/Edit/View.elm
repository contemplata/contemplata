module Edit.View exposing (view)


import Char
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Svg as Svg
import Svg.Attributes as Svg
-- import Svg as SVG -- to avoid ambiguity in a couple of places
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
import Edit.Config as AnnoCfg
import Edit.Model as M
import Edit.Core as C
import Edit.Command as Cmd
import Edit.Message as Msg
import Edit.Message.Core exposing (Msg(..))
import Edit.Popup as Popup
import Server


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
      , viewWindow M.Top model
      ] ++ viewSideWindow M.Top model ++
      [ viewWindow M.Bot model
      ] ++ viewSideWindow M.Bot model
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
viewWindow : M.Focus -> M.Model -> Html.Html Msg
viewWindow win model =
  Html.div
  [ backMouseDown win
  , winOnFocus win

  -- , topOnResize
--   , case win of
--       M.Top -> Atts.autofocus True
--       M.Bot -> Atts.autofocus False

--   , case win of
--       M.Top -> Atts.id "top"
--       M.Bot -> Atts.id "bot"

  , Atts.id <| case win of
      M.Top -> Cfg.windowName True
      M.Bot -> Cfg.windowName False

  -- @tabindex required to make the div propagate the keyboard events
  -- (see the `view` function)
  , Atts.attribute "tabindex" "1"

  , Atts.style
    [ "position" :> "absolute"
    -- , "width" :> (toString (100 - Cfg.sideSpace) ++ "%") -- "100%"
    , "width" :> (toString model.dim.widthProp ++ "%")
    , "height" :> case win of
        M.Top -> toString model.dim.heightProp ++ "%"
        M.Bot -> toString (100 - model.dim.heightProp) ++ "%"
    , case win of
        M.Top -> "top" :> "0"
        M.Bot -> "bottom" :> "0"
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

  [ viewTree win model
  , viewBottomLine win model ]
  ++ if (win == M.Bot && model.dim.heightProp <= 0) ||
        (win == M.Top && model.dim.heightProp > 0)
     then [viewMenu model] -- model.fileId]
     else []


-- | Determine the background color.
backColor : M.Focus -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"


-- | View tree in the specified window.
viewTree : M.Focus -> M.Model -> Html.Html Msg
viewTree focus model =

  let

    win = M.selectWin focus model
    treeId = M.getReprId focus win.tree model
    tree = M.getTree focus treeId model

  in

    drawTree
      focus win.selMain win.selAux
      <| markMisplaced first
      -- <| positionTree (M.getPosition win)
      <| positionTree (M.getPosition focus model)
      <| R.withWidth stdWidth Cfg.stdMargin tree


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
viewBottomLine : M.Focus -> M.Model -> Html.Html Msg
viewBottomLine win model =
  case (model.command, win == model.focus) of
    (Just cmd, True) -> viewCommand cmd win model
    _ -> viewTreeId win model


viewTreeId : M.Focus -> M.Model -> Html.Html Msg
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


viewCommand : String -> M.Focus -> M.Model -> Html.Html Msg
viewCommand pref win model =
  let
    cmdLst = Msg.cmdsWithPrefix pref
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

    mkMenuElem = Cmd.mkMenuElem model.ctrl
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
        , levelElem M.Segmentation
        , levelElem M.Syntax
        , levelElem M.Temporal
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
        ( if model.annoLevel == M.Temporal
          then temporalCommands
          else if model.annoLevel == M.Segmentation
               then segmentationCommands
               else syntaxCommands )
        -- [ Cmd.mkMenuItem Dummy (Just "Is CTRL down") (plainText isCtrl) ]


---------------------------------------------------
-- Drawing trees
---------------------------------------------------


drawTree
   : M.Focus -- ^ Which window is it in?
  -> Maybe C.NodeId -- ^ Selected main
  -> S.Set C.NodeId -- ^ Selected auxiliaries
  -> R.Tree ((M.Node, Position), NodeTyp) -- ^ Tree to draw
  -> Html.Html Msg
drawTree focus selMain selAux (R.Node ((node, pos), mark) subTrees) =
  let
    lineCfg = { defLineCfg
      | strokeWidth = 2
      , opacity = "0.7" }
      -- , color = "gray" }
    drawForest forest = case forest of
      [] -> []
      t :: ts ->
        drawTree focus selMain selAux t
          :: viewLine lineCfg pos (second <| first <| R.label t)
          :: drawForest ts
  in
    Html.div []
      (  drawNode node selMain selAux focus pos mark
      :: drawForest subTrees )


-- | Draw a tree node.
drawNode
   : M.Node
  -> Maybe C.NodeId
  -> S.Set C.NodeId
  -> M.Focus
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawNode node =
    case node of
        M.Node r -> drawInternal r
        M.Leaf r -> drawLeaf r


-- | Draw an internal tree node.
drawInternal
   : M.InternalNode
  -> Maybe C.NodeId
  -> S.Set C.NodeId
  -> M.Focus
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawInternal node selMain selAux focus at mark =
  let
    -- width = nodeWidth
    intNode = M.Node node
    width = stdWidth intNode
    height = Cfg.nodeHeight
    -- nodeId = Lens.get M.nodeId node
    nodeId = node.nodeId
    auxStyle =
      ( if S.member nodeId selAux || Just nodeId == selMain
        then ["background-color" :> "#BC0000"]
        else if mark == Misplaced
        then ["background-color" :> "#EF597B"]
        else ["background-color" :> "#3C8D2F"] )
      ++
      ( if Just nodeId == selMain
          then ["border" :> "solid", "border-color" :> "black"]
          else ["border" :> "none"] )
    htmlLeaf =
        [ Html.text node.nodeVal
        , case node.nodeTyp of
            Nothing -> Html.sub [] []
            Just en -> Html.sub []
                       [ Html.text <|
                         String.map Char.toUpper <|
                         String.left 2 en.name ]
--             Just (M.NodeEvent _) -> Html.sub [] [Html.text "EV"]
--             Just (M.NodeSignal _) -> Html.sub [] [Html.text "SI"]
--             Just (M.NodeTimex _) -> Html.sub [] [Html.text "TX"]
        ]
  in
    Html.div
      [ nodeMouseDown focus intNode
      , Atts.class "noselect"
      , Atts.style <| auxStyle ++
          [ "cursor" :> "pointer"
          -- , "opacity" :> "1.0"

          , "width" :> px width
          , "height" :> px height
          , "border-radius" :> "40%" -- "4px"
          , "position" :> "absolute"
          -- , "left" :> px (at.x - nodeWidth // 2)
          -- , "top" :> px (at.y - nodeHeight // 2)
          , "left" :> px (at.x - width // 2)
          , "top" :> px (at.y - height // 2)

          , "color" :> "white"
          , "display" :> "flex"
          , "align-items" :> "center"
          , "justify-content" :> "center"
          ]
      ]
      [Html.p [] htmlLeaf]
      -- htmlLeaf
--       [ Html.div
--           [ Atts.attribute "contenteditable" "true" ]
--           [ Html.text node.nodeVal ]
--       ]


-- | Draw a leaf tree node.
drawLeaf
   : M.LeafNode
  -> Maybe C.NodeId
  -> S.Set C.NodeId
  -> M.Focus
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawLeaf node selMain selAux focus at mark =
  let
    -- width = nodeWidth
    leafNode = M.Leaf node
    width = stdWidth leafNode
    height = Cfg.nodeHeight
    nodeId = node.nodeId
    auxStyle =
      ( if S.member nodeId selAux || Just nodeId == selMain
        then ["background-color" :> "#BC0000"]
        else if mark == Misplaced
        then ["background-color" :> "#EF597B"]
        else ["background-color" :> "#1F5C9A"] ) -- "#1F9A6D"
      ++
      ( if Just nodeId == selMain
          then ["border" :> "solid", "border-color" :> "black"]
          else ["border" :> "none"] )
    htmlLeaf =
        [ Html.text node.nodeVal
        -- [ Html.text (sent node.leafPos).orth
        , Html.sub [] [Html.text <| toString node.leafPos] ]
  in
    Html.div
      [ nodeMouseDown focus leafNode
      , Atts.class "noselect"
      , Atts.style <| auxStyle ++
          [ "cursor" :> "pointer"
          -- , "opacity" :> "1.0"

          , "width" :> px width
          , "height" :> px height
          , "border-radius" :> "40%" -- "4px"
          , "position" :> "absolute"
          -- , "left" :> px (at.x - nodeWidth // 2)
          -- , "top" :> px (at.y - nodeHeight // 2)
          , "left" :> px (at.x - width // 2)
          , "top" :> px (at.y - height // 2)

          , "color" :> "white"
          , "display" :> "flex"
          , "align-items" :> "center"
          , "justify-content" :> "center"
          ]
      ]
      [Html.p [] htmlLeaf]
      -- htmlLeaf
--       [ Html.div
--           [ Atts.attribute "contenteditable" "true" ]
--           [ Html.text node.nodeVal ]
--       ]


---------------------------------------------------
-- Side windows
---------------------------------------------------


-- | The view of a side window.
viewSideWindow : M.Focus -> M.Model -> List (Html.Html Msg)
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
    -> M.Focus
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
            M.Top -> toString dim.heightProp ++ "%"
            M.Bot -> toString (100 - dim.heightProp) ++ "%"
        , "right" :> "0"
        , case win of
            M.Top -> "top" :> "0"
            M.Bot -> "bottom" :> "0"
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
              M.Top -> True
              M.Bot -> False
      ]
    topChildren = [viewSideMenu win model]
  in
    -- `topChildren` after `children` so that they stay on top,
    -- if I remember correctly
    div (children ++ topChildren)


viewSideMenu : M.Focus -> M.Model -> Html.Html Msg
viewSideMenu focus model =
  let

    topHeight = (model.dim.height * model.dim.heightProp) // 100
    pos = case focus of
      M.Top -> 0
      M.Bot -> topHeight

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
viewSideEditLabel : M.Focus -> M.Model -> Html.Html Msg
viewSideEditLabel win model =
  let
    selected = (Lens.get (M.windowLens win) model).selMain
--     selected = case win of
--       M.Top -> model.top.selMain
--       M.Bot -> model.bot.selMain
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
            M.Top -> Cfg.editLabelName True
            M.Bot -> Cfg.editLabelName False
        ] ++ condAtts
      )
      []
  in
    inp


-- | The view of the side edit window -- the label.
viewSideEditLeaf
    : M.Model -> M.Focus -> C.NodeId -> M.InternalNode -> List (Html.Html Msg)
viewSideEditLeaf model focus nodeId node =
  let

    inpLabel = textGenericGen
               (SetNodeAttr nodeId focus)
               Anno.NodeLabelAttr
               "Label: "
               node.nodeVal
               (Just <| case focus of
                            M.Top -> Cfg.editLabelName True
                            M.Bot -> Cfg.editLabelName False)

    inpComment = textGenericGen
                 (SetNodeAttr nodeId focus)
                 Anno.NodeCommentAttr
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
    -> M.Focus
    -> C.NodeId
    -> M.NodeTyp
    -> M.InternalNode
    -> List (Html.Html Msg)
viewSideEditInternal model focus nodeId nodeTyp node =
  let

    labelSet = if nodeTyp == M.PosNode
               then Anno.preTerminalLabelSet
               else if nodeTyp == M.Phrasal
                    then Anno.phrasalLabelSet
                    else Anno.nodeLabelSet
    inpLabel = inputGenericConstrainedGen
               (SetNodeAttr nodeId focus)
               "Label"
               node.nodeVal
               labelSet
               Anno.NodeLabelAttr
               (Just <| case focus of
                            M.Top -> Cfg.editLabelName True
                            M.Bot -> Cfg.editLabelName False)

    inpComment = textGenericGen
                 (SetNodeAttr nodeId focus)
                 Anno.NodeCommentAttr
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


-- viewSideEvent : M.Focus -> C.NodeId -> Anno.Event -> List (Html.Html Msg)
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
-- viewSideSignal : M.Focus -> C.NodeId -> Anno.Signal -> List (Html.Html Msg)
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
-- viewSideTimex : M.Model -> M.Focus -> C.NodeId -> M.InternalNode -> Anno.Timex -> List (Html.Html Msg)
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
    -> M.Focus
    -> C.NodeId
    -> M.InternalNode
    -> Anno.Entity
    -> List (Html.Html Msg)
viewSideEntity model focus nodeId node ent =
  let

    -- inputGeneric = inputGenericGen (SetTimexAttr nodeId focus)
    inputGeneric attrName = inputGenericGen (SetEntityAttr nodeId focus attrName) attrName

    -- inpCalendar = inputGeneric "Calendar" ti.tiCalendar Anno.timexCalendarStr Anno.TiCalendarAttr
    inputAttr (attrName, val) =
        inputGeneric attrName
            val -- (Anno.getAttr attr ent)
            (AnnoCfg.attrConfig ent.name attrName model.annoConfig) -- Anno.timexCalendarStr
            -- Anno.TiCalendarAttr

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

  in
      [ Html.hr [] []
      -- , Html.h3 [] [Html.text "Timex:"]
      , Html.text <| ent.name ++ ":"
      , Html.table [] <|
          L.map inputAttr (D.toList ent.attributes)
--             [ inpCalendar, inpFunctionInDocument, inpPred, inpType
--             , inpTemporalFunction, inpLingValue, inpValue, inpMod
--             , inpAnchor ] ++ typeDependent
      ]


-- | The view of a side window.
viewSideEdit : Bool -> M.Focus -> M.Model -> Html.Html Msg
viewSideEdit visible win model =
  let

    selected = (Lens.get (M.windowLens win) model).selMain
--     selected = case win of
--       M.Top -> model.top.selMain
--       M.Bot -> model.bot.selMain

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


-- -- | The view of a side window.
-- viewSideContext
--     : Bool           -- ^ Visible?
--     -> M.Focus
--     -> M.Model
--     -> Html.Html Msg
-- viewSideContext visible foc model =
--   let
--     treeSelected = M.getReprId (M.selectWin foc model).tree model
--     viewTree spks (treeId, mayWho) =
--       let sent = case D.get treeId model.file.sentMap of
--                    Nothing -> ""
--                    Just x -> x
--           treeIdRepr = M.getReprId treeId model
--           isSelected = treeIdRepr == treeSelected
--       in  viewSent foc isSelected treeIdRepr sent spks mayWho
--     viewTurn turn = List.map (viewTree turn.speaker) (D.toList turn.trees)
--     div = viewSideDiv visible foc model
--       [ Html.ul
--           [Atts.style
--              [ "position" :> "absolute"
--              , "top" :> px Cfg.sideMenuHeight
--              ]
--           ]
-- --           (List.map
-- --              (\(treeId, (sent, _)) -> viewSent foc (treeId == treeSelected) treeId sent)
-- --              (D.toList model.trees)
-- --           )
--           (List.concat <| List.map viewTurn model.file.turns)
-- --       , Html.p
-- --           [ Atts.id <| Cfg.selectSentName True
-- --           , Atts.style ["position" :> "absolute", "bottom" => px 0] ]
-- --           [Html.text "test text"]
--       ]
--   in
--     div


-- viewSent
--   : M.Focus -- ^ Where is the focus on
--   -> Bool -- ^ Is the tree currently viewed?
--   -> C.TreeId -- ^ The tree ID (the representative) ...
--   -> M.Sent -- ^ ... and the sentence corresponding to the tree
--   -> List String -- ^ Speakers of the turn
--   -> Maybe Int -- ^ Who is speaking now
--   -> Html.Html Msg
-- viewSent foc isSelected treeId sent spks who =
--   let
--     paraAtts = if isSelected
--       then [ Atts.style ["font-weight" :> "bold"] ]
--       else []
--     liAtts = if isSelected
--       then [ Atts.id <| Cfg.selectSentName <|
--                  case foc of
--                      M.Top -> True
--                      M.Bot -> False ]
--       else []
--     spk = case who of
--             Nothing ->
--               case spks of
--                 (x :: _) -> x
--                 _ -> "?"
--             Just i ->
--               case List.head (List.drop (i-1) spks) of
--                 Just x -> x
--                 Nothing -> "?"
--     para = Html.p
--       -- [Atts.style ["font-weight" :> "bold"]]
--       paraAtts
--       -- [Html.text <| toString treeId ++ "." ++ spk ++ ": " ++ sent]
--       [Html.text <| spk ++ ": " ++ sent]
--     li = Html.li liAtts <| Util.single <|
--       Html.div
--         [ Atts.class "noselect"
--         , Events.onClick (SelectTree foc treeId)
--         , Atts.style ["cursor" :> "pointer"]
--         ]
--         [para]
--   in
--     li


-- | The view of a side window.
viewSideContext
    : Bool           -- ^ Visible?
    -> M.Focus
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
  : M.Focus   -- ^ Where is the focus on
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
                     M.Top -> True
                     M.Bot -> False ]
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
    : M.Focus     -- ^ Model focus
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


-- viewSent
--   : M.Focus -- ^ Where is the focus on
--   -> Bool -- ^ Is the tree currently viewed?
--   -> C.TreeId -- ^ The tree ID (the representative) ...
--   -> M.Sent -- ^ ... and the sentence corresponding to the tree
--   -> List String -- ^ Speakers of the turn
--   -> Maybe Int -- ^ Who is speaking now
--   -> Html.Html Msg
-- viewSent foc isSelected treeId sent spks who =
--   let
--     paraAtts = if isSelected
--       then [ Atts.style ["font-weight" :> "bold"] ]
--       else []
--     liAtts = if isSelected
--       then [ Atts.id <| Cfg.selectSentName <|
--                  case foc of
--                      M.Top -> True
--                      M.Bot -> False ]
--       else []
--     spk = case who of
--             Nothing ->
--               case spks of
--                 (x :: _) -> x
--                 _ -> "?"
--             Just i ->
--               case List.head (List.drop (i-1) spks) of
--                 Just x -> x
--                 Nothing -> "?"
--     para = Html.p
--       paraAtts
--       [Html.text <| spk ++ ": " ++ sent]
--     li = Html.li liAtts <| Util.single <|
--       Html.div
--         [ Atts.class "noselect"
--         , Events.onClick (SelectTree foc treeId)
--         , Atts.style ["cursor" :> "pointer"]
--         ]
--         [para]
--   in
--     li


---------------------------------------------------
-- Side log (messages)
---------------------------------------------------


-- | The view of a side window.
viewSideLog : Bool -> M.Focus -> M.Model -> Html.Html Msg
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
  : M.Focus -- ^ Where is the focus on
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
        linkSet = Lens.get (M.fileLens M.Top => M.linkSet) model
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
  -> (M.Link, M.LinkData)
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
  -> (M.Link, M.LinkData)
  -> List (Html.Html Msg)
viewLink_ model ((from, to), linkData) =
  let
    -- top = model.top
    top = Lens.get (M.windowLens M.Top) model
    -- bot = model.bot
    bot = Lens.get (M.windowLens M.Bot) model
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
    getReprId = M.getReprId M.Top

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

    nodePos1 nodeId pos tree = nodePos nodeId
      <| positionTree pos
      <| R.withWidth stdWidth Cfg.stdMargin tree
    posIn addr foc win shift = Maybe.andThen shift <| nodePos1
      (second addr)
      (M.getPosition foc model)
      (M.getTree foc (M.getReprId foc win.tree model) model)

    fromPos =
      if first from == M.getReprId M.Top top.tree model
      then posIn from M.Top top shiftTop
      else posIn from M.Bot bot shiftBot
    toPos = -- posIn to bot shiftBot
      if first to == M.getReprId M.Top bot.tree model
      then posIn to M.Bot bot shiftBot
      else posIn to M.Top top shiftTop
    signPos = case signalMay of
      Nothing -> Nothing
      Just addr ->
        if first addr == M.getReprId M.Top bot.tree model
        then posIn addr M.Bot bot shiftBot
        else posIn addr M.Top top shiftTop

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
          [ viewLine lineCfg lin1.beg lin1.end
          , viewLine lineCfg lin2.beg lin2.end
          , drawCircle circleCfg endCirc
          , drawLinkCircle model (from, to) midCirc ]
          ++ case lin3May of
               Nothing -> []
               Just lin3 -> [viewLine lineCfg lin3.beg lin3.end]
      _ -> []


-- | Draw the circle which represents the relation.
drawLinkCircle
    : M.Model
    -> M.Link
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
-- Lines
---------------------------------------------------


type alias LineCfg =
  { color : String
  , strokeWidth : Int
  , zindex : Int
  , strokeDasharray : Maybe String
  , opacity : String
  , isArrow : Bool
  }


defLineCfg : LineCfg
defLineCfg =
  { color = "black"
  , strokeWidth = 1
  , zindex = -1
  , strokeDasharray = Nothing
  , opacity = "1"
  , isArrow = False
  }


viewLine : LineCfg -> Position -> Position -> Html.Html Msg
viewLine cfg beg end =
  let
    -- Note that the width is handled in a tricky way. This is to handle the
    -- case where the line is vertical. The case where the line is horizontal is
    -- not handled.
    width  = (\x->x+1) <| abs <| end.x - beg.x
    height = abs <| end.y - beg.y
    (x1, x2) = case end.x >= beg.x of
             True  -> ("1", toString width)
             False -> (toString width, "1")
    (y1, y2) = case end.y >= beg.y of
             True  -> ("0", toString height)
             False -> (toString height, "0")
    dash = case cfg.strokeDasharray of
      Nothing -> []
      Just x  -> [Svg.strokeDasharray x]
    line = Svg.line
      ( [ Svg.stroke cfg.color
        , Svg.strokeWidth (toString cfg.strokeWidth)
        , Svg.opacity cfg.opacity
        , Svg.x1 x1, Svg.y1 y1, Svg.x2 x2, Svg.y2 y2 ]
        ++ dash
      )
      []
    svg = Svg.svg
      [ Svg.width (toString <| (\x->x+1) <| width)
      , Svg.height (toString height)
      ] [line]
  in
    Html.div
      [ Atts.style
          [ "position" :> "absolute"
          , "left" :> px (min beg.x end.x)
          , "top" :> px (min beg.y end.y)
          , "pointer-events" :> "none"
          , "z-index" :> toString cfg.zindex
          ]
      ]
      [svg]


---------------------------------------------------
-- Events
---------------------------------------------------


nodeMouseDown : M.Focus -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win <| Lens.get M.nodeId x)


backMouseDown : M.Focus -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)


-- backDoubleClick : M.Focus -> Html.Attribute Msg
-- backDoubleClick win =
--   Events.onDoubleClick (Focus win)


winOnFocus : M.Focus -> Html.Attribute Msg
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
        Nothing -> mainKeyDown model.ctrl
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
  : Bool -- ^ CTRL down?
  -> Html.Attribute Msg
mainKeyDown ctrl =
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
          case Cmd.msgFromKeyCode ctrl code of
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
-- Positioning
---------------------------------------------------


-- | Position a given tree. This function calculates the positions of the
-- individual nodes in the given tree, based on their widths (see also
-- `R.withWidth`).
positionTree : Position -> R.Tree (M.Node, R.Width) -> R.Tree (M.Node, Position)
positionTree pos (R.Node (node, rootWidth) subTrees) =
  let
    forestWidth = List.sum <| L.map (R.label >> second) subTrees
    positionF w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = second <| R.label t
          tpos = {x = w0 + tw // 2, y = pos.y + Cfg.moveDown}
        in
          positionTree tpos t :: positionF (w0 + tw) ts
  in
    R.Node (node, pos) (positionF (pos.x - forestWidth // 2) subTrees)


-- | Retrieve the position of a node in a given tree.
nodePos : C.NodeId -> R.Tree (M.Node, Position) -> Maybe Position
nodePos nodeId tree = Maybe.map second <|
  Util.find
    (\node -> Lens.get M.nodeId (first node) == nodeId)
    (R.flatten tree)


---------------------------------------------------
-- Determine which nodes should be marked
-- as "misplaced" or "non-projective"
---------------------------------------------------


-- | To mark nodes as misplaced.
type NodeTyp = Normal | Misplaced


-- | Determine node spans.
addSpans
    : (a -> M.Node)
    -> R.Tree a
    -> R.Tree (a, (Int, Int))
addSpans getNode =
    let
        go (R.Node wrapper forest0) =
            case getNode wrapper of
                M.Node _ ->
                    let forest = L.map go forest0
                        beg = spanBeg forest
                        end = spanEnd forest
                        span = (beg, end)
                    in  R.Node (wrapper, span) forest
                M.Leaf r ->
                    let span = (r.leafPos, r.leafPos)
                    in  R.Node (wrapper, span) []
        spanBeg xs =
            case L.minimum <| L.map (first << second << R.label) xs of
                Nothing -> Debug.crash "Edit.View.addSpans: spanBeg"
                Just x  -> x
        spanEnd xs =
            case L.maximum <| L.map (second << second << R.label) xs of
                Nothing -> Debug.crash "Edit.View.addSpans: spanEnd"
                Just x  -> x
    in
        go


-- | Determine which leaves should be marked as "misplaced".
markMisplaced
    : (a -> M.Node)
    -> R.Tree a
    -> R.Tree (a, NodeTyp)
markMisplaced getNode =
    let
        markTree prevSpan (R.Node (wrapper, span) forest) =
            let
                nodeTyp = case prevSpan of
                    Nothing -> Normal
                    Just (_, prevEnd) ->
                        if prevEnd < first span
                        then Normal
                        else Misplaced
            in
                (Just span, R.Node (wrapper, nodeTyp) (markForest forest))
        markForest =
            second << Util.mapAccumL markTree Nothing
        markRoot (R.Node (wrapper, span) forest) =
            R.Node (wrapper, Normal) (markForest forest)
    in
        propagateMarks << markRoot << addSpans getNode


-- | Propagate the markings downward to the leaves.
propagateMarks
    : R.Tree (a, NodeTyp)
    -> R.Tree (a, NodeTyp)
propagateMarks =
    let
        propTyp typ =
            case typ of
                Normal -> Nothing
                x -> Just x
        markTree fromUp (R.Node (wrapper, typ) forest) =
            case fromUp of
                Nothing ->
                    let newTyp = propTyp typ
                    in  R.Node (wrapper, typ) (markForest newTyp forest)
                Just newTyp ->
                    R.Node (wrapper, newTyp) (markForest fromUp forest)
        markForest : Maybe NodeTyp -> R.Forest (a, NodeTyp) -> R.Forest (a, NodeTyp)
        markForest fromUp = L.map (markTree fromUp)
    in
        markTree Nothing


-- -- | Determine which leaves should be marked as "misplaced".
-- markMisplacedLeaves
--     : (a -> M.Node)
--     -> R.Tree a
--     -> R.Tree (a, NodeTyp)
-- markMisplacedLeaves getNode =
--     let
--         -- Leave only the highest position in the set
--         trim s = case L.maximum (S.toList s) of
--                      Just x -> S.singleton x
--                      Nothing -> Debug.crash
--                        "markMisplacedLeaves: the set should never be empty!"
--         -- The leaf is marked as normal if the accumulator is a set of
--         -- consecutive numbers. Otherwise, it is marked as misplaced.
--         whatTyp pos acc =
--             let
--                 newAcc = S.insert pos acc
--             in
--                 if isConsecutive (S.toList newAcc)
--                 then (trim newAcc, Normal)
--                 else (newAcc, Misplaced)
--         update acc wrapper =
--             case getNode wrapper of
--                 M.Node r -> (acc, (wrapper, Normal))
--                 M.Leaf r ->
--                     let (newAcc, nodeTyp) = whatTyp r.leafPos acc
--                     in  (newAcc, (wrapper, nodeTyp))
--     in
--         second << R.mapAccum update S.empty
--
--
-- -- | Is the given list of numbers consecutive, i.e.,
-- -- [k, k+1, k+2, ...].
-- isConsecutive : List Int -> Bool
-- isConsecutive theList =
--     case theList of
--         (x :: y :: xs) ->
--             x+1 == y && isConsecutive (y :: xs)
--         [] -> True
--         [x] -> True


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


-- | Doubly generic input field...
inputGenericGen
    : (Anno.Attr -> Msg)
    -- ^ Message to send when an attribute is selected
    -> String
    -- ^ Attribute name
    -> Anno.Attr
    -- ^ The chosen attribute value
    -> AnnoCfg.Attr
    -- ^ The configuration corresponding to the attribute
    -> Html.Html Msg
inputGenericGen setAttr label attrValue attrCfg =
  let
    labelTD =
        Html.td
            [ Atts.class "noselect"
            , Atts.align "right" ]
            [Html.text (label ++ ": ")]
  in
    case (attrValue, attrCfg) of
        (Anno.Anchor, AnnoCfg.Anchor) ->
            Html.tr []
              [ labelTD
              , Html.td
                  [ Atts.class "noselect"
                  , Atts.align "right"
                  ]
                  [Html.text "ANCHOR (IMPLEMENT)"]
              ]
        (Anno.Attr value, AnnoCfg.Closed r) ->
            let
              setMsg = setAttr << Anno.Attr
              option val = Html.option
                [ Atts.value val
                , Atts.selected (val == value) ]
                [ Html.text val ]
            in
              Html.tr []
                [ labelTD
                , Html.td []
                    [Html.select
                         [ Events.on "change" (Decode.map setMsg Events.targetValue)
                         , blockKeyDownEvents ]
                         (List.map option r.among)
                    ]
                ]
        _ ->
            Html.tr []
              [ labelTD
              , Html.td
                  [ Atts.class "noselect"
                  , Atts.align "right"
                  ]
                  [Html.text <| "TODO: " ++ toString (attrValue, attrCfg)]
              ]


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
--     -> M.Focus
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


---------------------------------------------------
-- Configuration
---------------------------------------------------


-- | Width of a node.
stdWidth : M.Node -> Int
stdWidth x =
  -- let val = Lens.get M.nodeVal x
  let
    (txt, ix) = case x of
      M.Node r -> (r.nodeVal, "")
      M.Leaf r -> (r.nodeVal, toString r.leafPos)
  in  max 30 <| String.length txt * 10 + String.length ix * 6
-- stdWidth x = 100
