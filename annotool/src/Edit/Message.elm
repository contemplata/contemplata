module Edit.Message exposing (Msg(..), update, dummy, cmdsWithPrefix)


import String
import List as L
import Dict as D
import Set as S
import Mouse exposing (Position)
import Task as Task
import Dom as Dom
import Dom.Scroll
import Focus exposing ((=>))
import Focus as Focus
import Window as Window
import WebSocket
import Json.Decode as Decode
import Focus as Lens
import Either exposing (..)

import Rose as R

import Config as Cfg
import Edit.Model as M
import Edit.Core as C
import Edit.Anno as Anno
import Edit.Rule as Rule
import Edit.Popup as Popup
-- import Edit.Command as Cmd
import Menu
import Server
import Util


type Msg
  = DragStart M.Focus Position
    -- ^ Neither `DragAt` nor `DragEnd` have their focus. This is on purpose.
    -- Focus should be determined, in their case, on the basis of the drag in
    -- the underlying model. We do not support concurrent drags at the moment.
  | DragAt Position
  | DragEnd Position
  | Select M.Focus C.NodeId
  | SelectTree M.Focus C.TreeId
  | SelectLink M.Link
  | Focus M.Focus
  | Resize Window.Size -- ^ The height and width of the entire window
  | Increase Bool Bool -- ^ Change the proportions of the window
  | Previous
  | Next
  | ChangeLabel C.NodeId M.Focus String
  | EditLabel
  | Delete -- ^ Delete the selected nodes in the focused window
  | DeleteTree
    -- ^ Delete the selected nodes in the focused window
    -- together with the corresponding subtrees
  | Add -- ^ Delete the selected nodes in the focused window
  -- | ChangeType -- ^ Change the type of the selected node
  | MkSignal -- ^ Create signal
  | MkEvent -- ^ Create event
  | MkTimex -- ^ Create event
  | ParseRaw Bool  -- ^ Reparse from scratch the sentence in focus; the argument determines
                   -- wheter pre-processing should be used or not
  | ParseSent Server.ParserTyp  -- ^ Reparse the sentence in focus
  | ParseSentPos Server.ParserTyp -- ^ Reparse the sentence in focus, preserve POList (String, String)S tags
  | ParseSentCons Server.ParserTyp  -- ^ Reparse the sentence in focus with the selected nodes as constraints
  | ApplyRules -- ^ Apply the (flattening) rules
  | CtrlDown
  | CtrlUp
  | Connect
  | Attach
  | Swap Bool
  | Files -- ^ Go back to files menu
  | SaveFile  -- ^ Save the current file
  | ConcatWords  -- ^ Merge two (or more) words
  | Join  -- ^ Merge the two trees in view
  | Undo
  | Redo
  | SideMenuEdit M.Focus
  | SideMenuContext M.Focus
  | ShowContext
  | SideMenuLog M.Focus
  -- * Modifying general node's attributes
  | SetNodeAttr C.NodeId M.Focus Anno.NodeAttr
  -- * Event modification events...
  | SetEventAttr C.NodeId M.Focus Anno.EventAttr
--   | SetEventClass C.NodeId M.Focus Anno.EventClass
--   | SetEventType C.NodeId M.Focus Anno.EventType
--   | SetEventTime C.NodeId M.Focus (Maybe Anno.EventTime)
--   | SetEventAspect C.NodeId M.Focus (Maybe Anno.EventAspect)
  | SetSignalAttr C.NodeId M.Focus Anno.SignalAttr
  | SetTimexAttr C.NodeId M.Focus Anno.TimexAttr
  | CommandStart
  | CommandEnter
  | CommandEscape
  | CommandBackspace
  | CommandComplete
  | CommandChar Char
  | CommandString String
  | Popup              -- ^ Open a popup window
      Popup.Popup
      (Maybe String)   -- ^ The (optionl) HTML ID to focus on
  | QuitPopup
  | SplitBegin
  | SplitChange Int
  | SplitFinish Int
  -- | Goto C.Addr -- ^ Move to a given node in the focused window
  | Many (List Msg)
--     -- ^ Tests
--   | TestInput String
--   | TestGet String
--   | TestSend


-- update : Msg -> M.Model -> ( M.Model, Cmd Msg )
-- update msg model =
--   ( updateHelp msg model, Cmd.none )


update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =

 let

  idle x = (x, Cmd.none)
  onFst f (x, y) = (f x, y)

 in

  onFst (M.freezeHist << M.updateSelection) <| case msg of

    DragStart focus xy -> idle <|
      Focus.set
        (M.winLens focus => M.drag)
        (Just (M.Drag xy xy))
        model

    DragAt xy -> idle <|
      Focus.update
        (M.winLens (M.dragOn model) => M.drag)
        (Maybe.map (\{start} -> M.Drag start xy))
        model

    DragEnd _ -> idle
      <| Focus.update (M.winLens (M.dragOn model))
           (\win -> { win | drag = Nothing, pos = M.getPosition win})
      <| model

    Focus win -> idle <| {model | focus = win}

    Resize x -> idle <|
      Focus.update M.dim
        (\dim -> {dim | height=x.height, width=x.width})
        model

    Increase horizontalAxe flag -> idle <|
      let
        trim x = max 0 <| min 100 <| x
        change = case flag of
          True  -> Cfg.increaseSpeed
          False -> -Cfg.increaseSpeed
        propLens = if horizontalAxe then M.heightProp else M.widthProp
      in
        Focus.update
          (M.dim => propLens)
          (\x -> trim <| x + change)
          model
        -- {model | winProp = newProp}

    Select win i -> idle <|
      if model.ctrl
        then M.selectNodeAux win i model
        else M.selectNode win i model

    -- SelectTree win treeId -> idle <| model
    SelectTree win treeId -> idle <| M.moveCursorTo win treeId model

    SelectLink link ->
      let
        newLink =
          if Just link == model.selLink
          then Nothing
          else Just link
        newModel = {model | selLink = newLink}
        target = case model.focus of
          M.Top -> Cfg.windowName True
          M.Bot -> Cfg.windowName False
      in
        ( newModel
        , Task.attempt
            (\_ -> dummy)
            (Dom.focus target)
        )

    Next -> idle <| M.moveCursor True model

    Previous -> idle <| M.moveCursor False model

    ChangeLabel nodeId win newLabel ->
        idle <| M.setLabel nodeId win newLabel model

    EditLabel ->
      let
        target = case model.focus of
          M.Top -> Cfg.editLabelName True
          M.Bot -> Cfg.editLabelName False
      in
        ( Focus.set
            (M.winLens model.focus => M.side)
            M.SideEdit
            model
        , Task.attempt
            (\_ -> dummy)
            (Dom.focus target)
        )

    Delete -> idle <| M.deleteSel model.focus model

    DeleteTree -> idle <| M.deleteSelTree model.focus model

    Add -> idle <| M.addSel model.focus model

    -- ChangeType -> idle <| M.changeTypeSel model.focus model

    MkSignal -> idle <| M.mkSignalSel model.focus model
    MkEvent -> idle <| M.mkEventSel model.focus model
    MkTimex -> idle <| M.mkTimexSel model.focus model

    CtrlDown -> idle <| {model | ctrl=True}
    CtrlUp -> idle <| {model | ctrl=False}

    Connect -> idle <| M.connect model

    Attach -> idle <| M.attachSel model

    Swap left -> idle <| M.swapSel left model

    -- Files -> idle <| model -- ^ Handled upstream
    Files -> Debug.crash "Edit.Message.Files: should be handled upstream!"

    Join ->
      let
        treeTop = (M.selectWin M.Top model).tree
        treeBot = (M.selectWin M.Bot model).tree
        newModel = M.join treeTop treeBot model
      in
        idle newModel
        -- parseSent Server.Stanford newModel

    ParseRaw prep ->
      let
        treeId = (M.selectWin model.focus model).tree
        txtFor id = case D.get id model.file.sentMap of
                  Nothing -> ""
                  Just x -> x
        txt = String.join " "
              <| L.map txtFor
              <| S.toList
              <| M.getPart treeId model
        req = Server.ParseRaw model.fileId treeId txt prep
        send = Server.sendWS model.config req
      in
        (model, send)

    ParseSent parTyp -> parseSent parTyp model
--       let
--         treeId = (M.selectWin model.focus model).tree
--         tree = M.getTree treeId model
--         word node = case node of
--           M.Node _ -> Nothing
--           M.Leaf {nodeVal} -> Just nodeVal
--         words = List.reverse <| Util.catMaybes <| List.map word <| R.flatten tree
--         req = Server.encodeReq (Server.ParseSent model.fileId treeId parTyp words)
--         send = WebSocket.send Cfg.socketServer req
--       in
--         (model, send)

    ParseSentPos parTyp ->
      let
        treeId = (M.selectWin model.focus model).tree
        wordsPos = getWordPos (M.getTree treeId model)
        req = Server.ParseSentPos model.fileId treeId parTyp wordsPos
        send = Server.sendWS model.config req
      in
        (model, send)

    ParseSentCons parTyp ->
      let
        win = M.selectWin model.focus model
        treeId = win.tree
        tree = M.getTree treeId model
        -- wordsPos = getWordPos tree
        wordsPos = case getWordPos tree of
          Server.Single x -> x
          Server.Batch xs -> List.concat xs
        selection = M.selAll win
        span = getSpan selection tree
        req cns = Server.ParseSentCons model.fileId treeId parTyp cns wordsPos
        send cns = Server.sendWS model.config (req cns)
      in
        (model, send span)

    ApplyRules -> idle <|
      let
        wlen = M.winLens model.focus
        treeId = (Focus.get wlen model).tree
        (newTree, newSel) = Rule.apply Rule.theRule (M.getTree treeId model)
        updateSel win = {win | selAux = newSel, selMain = Nothing}
      in
        M.setTreeCheck model.fileId treeId newTree model
          |> Focus.update wlen updateSel

    -- Popup x -> idle <| {model | popup = Just x}
    Popup x targetMaybe ->
        let
            target = case targetMaybe of
                Just x  -> x
                Nothing -> Cfg.popupDivTemp
            focCmd = Task.attempt (\_ -> dummy) (Dom.focus target)
        in
            ( {model | popup = Just x}
            , focCmd )

    SaveFile ->
      let
        -- file = {treeMap = model.trees, turns = model.turns, linkSet = model.links}
        -- req = Server.SaveFile model.config.user model.fileId file
        req = Server.SaveFile model.config.user model.fileId model.file
        send = Server.sendWS model.config req
      in
        (model, send)

    ConcatWords -> parseSent Server.Stanford <| M.concatWords model

    Undo -> idle <| M.undo model
    Redo -> idle <| M.redo model

    SideMenuEdit focus -> idle <|
      Focus.set
        (M.winLens focus => M.side)
        M.SideEdit
        model

    SideMenuContext focus -> idle <|
      Focus.set
        (M.winLens focus => M.side)
        M.SideContext
        model

    ShowContext ->
        let task = Task.succeed (SideMenuContext model.focus)
        in  (model, Task.perform identity task)
--       Focus.set
--         (M.winLens model.focus => M.side)
--         M.SideContext
--         model

--     SideMenuContext focus ->
--       let
--         target = Cfg.sideDivName <| case focus of
--           M.Top -> True
--           M.Bot -> False
--       in
--         ( Focus.set
--             (M.winLens focus => M.side)
--             M.SideContext
--             model
--         , Task.attempt
--             (\_ -> dummy)
--             -- (Dom.focus target)
--             -- (Dom.Scroll.toTop target)
--             (Dom.Scroll.toY target 10000)
--         )

    SideMenuLog focus -> idle <|
      Focus.set
        (M.winLens focus => M.side)
        M.SideLog
        model

    SetNodeAttr nodeId focus attr -> idle <|
      case attr of
        Anno.NodeLabelAttr x -> M.setLabel nodeId focus x model
        Anno.NodeCommentAttr x -> M.setComment nodeId focus x model

    SetEventAttr nodeId focus attr -> idle <|
      case attr of
        Anno.ClassAttr x -> M.setEventAttr M.eventClass nodeId focus x model
        Anno.TypeAttr x -> M.setEventAttr M.eventType nodeId focus x model
        Anno.InquisitAttr x -> M.setEventAttr M.eventInquisit nodeId focus x model
        Anno.TimeAttr x -> M.setEventAttr M.eventTime nodeId focus x model
        Anno.AspectAttr x -> M.setEventAttr M.eventAspect nodeId focus x model
        Anno.PolarityAttr x -> M.setEventAttr M.eventPolarity nodeId focus x model
        Anno.MoodAttr x -> M.setEventAttr M.eventMood nodeId focus x model
        Anno.ModalityAttr x -> M.setEventAttr M.eventModality nodeId focus x model
        Anno.CardinalityAttr x -> M.setEventAttr M.eventCardinality nodeId focus x model
        Anno.ModAttr x -> M.setEventAttr M.eventMod nodeId focus x model
        Anno.PredAttr x -> M.setEventAttr M.eventPred nodeId focus x model
        -- Anno.CommentAttr x -> M.setEventAttr M.eventComment nodeId focus x model
        -- _ -> Debug.crash "SetEventAttr: not implemented yet!"

    SetSignalAttr nodeId focus attr -> idle <|
      case attr of
        Anno.SiTypeAttr x -> M.setSignalAttr M.signalType nodeId focus x model

    SetTimexAttr nodeId focus attr ->
      let
        setAnchor set =
            case set nodeId focus model of
                Left err ->
                    let popup = Popup.Info err
                    in  (model, firePopup popup Nothing)
                Right model -> idle model
      in
        case attr of
          Anno.TiAnchorAttr True -> setAnchor M.setTimexAnchor
          Anno.TiBeginPointAttr True -> setAnchor M.setTimexBeginPoint
          Anno.TiEndPointAttr True -> setAnchor M.setTimexEndPoint
          _ -> idle <| case attr of
            Anno.TiCalendarAttr x -> M.setTimexAttr M.timexCalendar nodeId focus x model
            Anno.TiTypeAttr x -> M.setTimexType nodeId focus x model
            Anno.TiFunctionInDocumentAttr x ->
                M.setTimexAttr M.timexFunctionInDocument nodeId focus x model
            Anno.TiPredAttr x -> M.setTimexAttr M.timexPred nodeId focus x model
            Anno.TiTemporalFunctionAttr x ->
                M.setTimexAttr M.timexTemporalFunction nodeId focus x model
            Anno.TiLingValueAttr x -> M.setTimexAttr M.timexLingValue nodeId focus x model
            Anno.TiValueAttr x -> M.setTimexAttr M.timexValue nodeId focus x model
            Anno.TiModAttr x -> M.setTimexAttr M.timexMod nodeId focus x model
            Anno.TiAnchorAttr True ->
                Debug.crash "Message: impossible happened (TiAnchorAttr True)!"
            Anno.TiAnchorAttr False -> M.remTimexAnchor nodeId focus model
            Anno.TiBeginPointAttr True ->
                Debug.crash "Message: impossible happened (TiBeginPointAttr True)!"
            Anno.TiBeginPointAttr False -> M.remTimexBeginPoint nodeId focus model
            Anno.TiEndPointAttr True ->
                Debug.crash "Message: impossible happened (TiEndPointAttr True)!"
            Anno.TiEndPointAttr False -> M.remTimexEndPoint nodeId focus model
            Anno.TiQuantAttr x -> M.setTimexAttr M.timexQuant nodeId focus x model
            Anno.TiFreqAttr x -> M.setTimexAttr M.timexFreq nodeId focus x model

    SplitBegin ->
        let pop x = (model, firePopup x Nothing) in
        case (M.selectWin model.focus model).selMain of
            Nothing -> pop (Popup.Info "You have to select a leaf")
            Just nodeId ->
                case M.getNode nodeId model.focus model of
                    M.Node _ -> pop (Popup.Info "You have to select a leaf")
                    M.Leaf r ->
                        let popup = Popup.Split {word=r.nodeVal, split=0}
                            focus = Just Cfg.splitSelectName
                        in (model, firePopup popup focus)
    SplitChange k -> idle <| M.changeSplit k model
    SplitFinish k -> idle <| M.performSplit k model

    CommandStart -> idle {model | command = Just ""}

    CommandChar c -> idle <|
      case model.command of
        Nothing -> model
        Just cmd -> {model | command = Just <| String.append cmd (String.fromChar c)}

    CommandString c -> idle <|
      case model.command of
        Nothing -> model
        Just cmd -> {model | command = Just <| String.append cmd c}

    CommandBackspace -> idle <|
      case model.command of
        Nothing -> model
        Just cmd -> {model | command = Just <| String.dropRight 1 cmd}

    CommandComplete -> idle <|
      case model.command of
        Nothing -> model
        Just cmd -> {model | command = Just <| complete cmd}

    CommandEscape -> idle <| {model | command = Nothing}

    CommandEnter ->
      let
        newModel = {model | command = Nothing}
      in
        model.command |>
        Maybe.andThen toMsg |>
        Maybe.map (\cmd -> (newModel, cmd)) |>
        Maybe.withDefault (idle newModel)

    QuitPopup ->
        let
            target =
                case model.focus of
                    M.Top -> Cfg.windowName True
                    M.Bot -> Cfg.windowName False
        in
            ( {model | popup = Nothing}
            , Task.attempt (\_ -> dummy) (Dom.focus target) )

    -- Goto addr -> idle <| M.goto addr model

    Many msgs ->
      let f msg (mdl0, cmds) =
        let (mdl, cmd) = update msg mdl0
        in  (mdl, cmd :: cmds)
      in
        let (mdl, cmds) = L.foldl f (model, []) msgs
        in  (mdl, Cmd.batch cmds)


-- | A dummy message.  Should avoid this...
dummy : Msg
dummy = Many []


firePopup
    : Popup.Popup
    -- ^ The popup to fire
    -> Maybe String
    -- ^ The (optional) name of the element to focus on
    -> Cmd Msg
firePopup popupRaw targetMaybe =
    let
        popup = Popup popupRaw targetMaybe
        popCmd = Task.perform identity (Task.succeed popup)
    in
        popCmd
--     let
--         popup = Popup popupRaw
--         popCmd = Task.perform identity (Task.succeed popup)
--         target = case targetMaybe of
--             Just x  -> x
--             Nothing -> Cfg.popupDivTemp
--         focCmd = Task.attempt (\_ -> dummy) (Dom.focus target)
--     in
--         Cmd.batch [popCmd, focCmd]


----------------------------------------------
-- Command
----------------------------------------------


-- | The list of commands.
cmdList : List (M.Command, Msg)
cmdList =
  [ ("save", SaveFile)
  , ("delete", Delete)
  , ("deltree", DeleteTree)
--   , ("add", Add)
  , ("restart", ParseRaw False)
  , ("preprocess", ParseRaw True)
  , ("parse", ParseSent Server.Stanford)
  , ("parsepos", ParseSentPos Server.Stanford)
  , ("dopparse", ParseSent Server.DiscoDOP)
  , ("dopparsepos", ParseSentPos Server.DiscoDOP)
  , ("parsecons", ParseSentCons Server.Stanford)
  , ("dopparsecons", ParseSentCons Server.DiscoDOP)
  , ("deepen", ApplyRules)
  , ("concat", ConcatWords)
  , ("connect", Connect)
  , ("join", Join)
  , ("split", SplitBegin)
--   , ("undo", Undo)
--   , ("redo", Redo)
  ]


-- | Translate a command into the corresponding model-related message.
toMsg : M.Command -> Maybe (Cmd Msg)
toMsg cmd0 =
  let
    exact = case List.filter (\(cmd, msg) -> cmd == cmd0) cmdList of
      [(cmd, msg)] -> Just <| Task.perform identity (Task.succeed msg)
      _ -> Nothing
    prefix = case cmdsWithPrefix_ cmd0 of
      (cmd, msg) :: _ -> Just <| Task.perform identity (Task.succeed msg)
      _ -> Nothing
  in
    case exact of
      Nothing -> prefix
      Just x  -> Just x



-- | Return all the commands beginning with the given prefix.
cmdsWithPrefix : String -> List M.Command
cmdsWithPrefix =
  List.map Tuple.first << cmdsWithPrefix_



-- | Return all the commands beginning with the given prefix.
cmdsWithPrefix_ : String -> List (M.Command, Msg)
cmdsWithPrefix_ prf =
  let p (cmd, msg) = String.startsWith prf cmd
  in  List.filter p cmdList


-- | Complete the command (if it's a prefix of other commands).
complete : String -> String
complete prf =
  let
    cmds = cmdsWithPrefix prf
  in
    case cmds of
      [] -> prf
      _ -> commonPrefix cmds


-- | Compute the longest common prefix.
commonPrefix : List String -> String
commonPrefix xs =
  case xs of
    [x] -> x
    hd :: tl -> commonPrefix2 hd (commonPrefix tl)
    [] -> Debug.crash "commonPrefix used on empty list"


-- | Compute the longest common prefix.
commonPrefix2 : String -> String -> String
commonPrefix2 s1 s2 =
  case (String.uncons s1, String.uncons s2) of
    (Just (c1, t1), Just (c2, t2)) ->
      if c1 == c2
      then String.cons c1 (commonPrefix2 t1 t2)
      else ""
    _ -> ""


----------------------------------------------
-- Retrieve the tree the POS tags
----------------------------------------------


type alias Orth = String
type alias Pos = String


-- | Retrieve the words and POS tags from a given tree.
getWordPos : R.Tree M.Node -> Server.ParseReq (List (Orth, Pos))
getWordPos tree0 =
  let
    go pos xs = case xs of
      [] -> []
      node :: nodes -> case node of
        M.Leaf r -> (r.nodeVal, pos) :: go "" nodes
        M.Node r -> go r.nodeVal nodes
    getList = go "" << List.reverse << R.flatten
    forest = R.subTrees tree0
  in
    case forest of
        [tree] -> Server.Single (getList tree)
        _ -> Server.Batch (List.map getList forest)


-- | Like `getWordPos` but just retrieves words.
getWords : R.Tree M.Node -> Server.ParseReq (List Orth)
getWords tree0 =
  let
    word node = case node of
      M.Node _ -> Nothing
      M.Leaf {nodeVal} -> Just nodeVal
    getList = List.reverse << Util.catMaybes << List.map word << R.flatten
    forest = R.subTrees tree0
  in
    case forest of
        [tree] -> Server.Single (getList tree)
        _ -> Server.Batch (List.map getList forest)


----------------------------------------------
-- Utils
----------------------------------------------


parseSent : Server.ParserTyp -> M.Model -> (M.Model, Cmd Msg)
parseSent parTyp model =
    let
        treeId = (M.selectWin model.focus model).tree
        words = getWords (M.getTree treeId model)
        req = Server.ParseSent model.fileId treeId parTyp words
        send = Server.sendWS model.config req
    in
        (model, send)


-- | Retrieve the span of a given node in a given tree.
getSpan1 : C.NodeId -> R.Tree M.Node -> Maybe (Int, Int)
getSpan1 nodeId tree =
    case getSpan (S.singleton nodeId) tree of
        x :: _ -> Just x
        [] -> Nothing


-- | Retrieve the span of a given node in a given tree.
getSpan : S.Set C.NodeId -> R.Tree M.Node -> List (Int, Int)
getSpan idSet =
     let
         span (val, (x, y)) =
             if S.member (Lens.get M.nodeId val) idSet
             then [(x, y)]
             else []
     in
         L.concatMap span << R.flatten << spanTree


-- | Add information about the span of the subtree for each node.
spanTree : R.Tree M.Node -> R.Tree (M.Node, (Int, Int))
spanTree (R.Node val ts) =
    case val of
        M.Leaf r -> R.Node (val, (r.leafPos, r.leafPos)) []
        M.Node r ->
            let ts1 = L.map spanTree ts
                span (R.Node val ts) = Tuple.second val
                get f = Maybe.withDefault 0 << f
                x = get L.minimum <| L.map (Tuple.first << span) <| ts1
                y = get L.maximum <| L.map (Tuple.second << span) <| ts1
             in R.Node (val, (x, y)) ts1
