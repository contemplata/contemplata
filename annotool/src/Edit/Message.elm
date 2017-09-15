module Edit.Message exposing (Msg(..), update, dummy, cmdsWithPrefix)


import String
import List as L
-- import Dict as D
import Set as S
import Mouse exposing (Position)
import Task as Task
import Dom as Dom
import Focus exposing ((=>))
import Focus as Focus
import Window as Window
import WebSocket
import Json.Decode as Decode
import Focus as Lens

import Rose as R

import Config as Cfg
import Edit.Model as M
import Edit.Anno as Anno
import Edit.Rule as Rule
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
  | Select M.Focus M.NodeId
  | SelectTree M.Focus M.TreeId
  | SelectLink M.Link
  | Focus M.Focus
  | Resize Window.Size -- ^ The height and width of the entire window
  | Increase Bool Bool -- ^ Change the proportions of the window
  | Previous
  | Next
  | ChangeLabel M.NodeId M.Focus String
  | EditLabel
  | Delete -- ^ Delete the selected nodes in the focused window
  | DeleteTree
    -- ^ Delete the selected nodes in the focused window
    -- together with the corresponding subtrees
  | Add -- ^ Delete the selected nodes in the focused window
  | ChangeType -- ^ Change the type of the selected node
  | ParseSent Server.ParserTyp  -- ^ Reparse the sentence in focus
  | ParseSentPos Server.ParserTyp -- ^ Reparse the sentence in focus, preserve POList (String, String)S tags
  | ParseSentCons  -- ^ Reparse the sentence in focus with the selected node as a source of constraints
  | ApplyRules -- ^ Apply the (flattening) rules
  | CtrlDown
  | CtrlUp
  | Connect
  | Attach
  | Swap Bool
  | Files -- ^ Go back to files menu
  | SaveFile  -- ^ Save the current file
  | ConcatWords  -- ^ Merge two (or more) words
  | Undo
  | Redo
  | SideMenuEdit M.Focus
  | SideMenuContext M.Focus
  | SideMenuLog M.Focus
  -- * Event modification events...
  | SetEventAttr M.NodeId M.Focus Anno.EventAttr
--   | SetEventClass M.NodeId M.Focus Anno.EventClass
--   | SetEventType M.NodeId M.Focus Anno.EventType
--   | SetEventTime M.NodeId M.Focus (Maybe Anno.EventTime)
--   | SetEventAspect M.NodeId M.Focus (Maybe Anno.EventAspect)
  | CommandStart
  | CommandEnter
  | CommandEscape
  | CommandBackspace
  | CommandComplete
  | CommandChar Char
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

    ChangeType -> idle <| M.changeTypeSel model.focus model

    CtrlDown -> idle <| {model | ctrl=True}
    CtrlUp -> idle <| {model | ctrl=False}

    Connect -> idle <| M.connect model

    Attach -> idle <| M.attachSel model

    Swap left -> idle <| M.swapSel left model

    Files -> idle <| model -- ^ Handled upstream

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
        req = Server.encodeReq (Server.ParseSentPos model.fileId treeId parTyp wordsPos)
        send = WebSocket.send Cfg.socketServer req
      in
        (model, send)

    ParseSentCons ->
      let
        win = M.selectWin model.focus model
        treeId = win.tree
        tree = M.getTree treeId model
        wordsPos = getWordPos tree
        selection = M.selAll win
        span = getSpan selection tree
        req cns = Server.encodeReq (Server.ParseSentCons model.fileId treeId cns wordsPos)
        send cns = WebSocket.send Cfg.socketServer (req cns)
      in
        (model, send span)

    ApplyRules -> idle <|
      let
        wlen = M.winLens model.focus
        treeId = (Focus.get wlen model).tree
        (newTree, newSel) = Rule.apply Rule.theRule (M.getTree treeId model)
        updateSel win = {win | selAux = newSel, selMain = Nothing}
      in
        M.setTree model.fileId treeId newTree model
          |> Focus.update wlen updateSel

    SaveFile ->
      let
        file = {treeMap = model.trees, turns = model.turns, linkSet = model.links}
        req = Server.encodeReq (Server.SaveFile model.fileId file)
        send = WebSocket.send Cfg.socketServer req
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

    SideMenuLog focus -> idle <|
      Focus.set
        (M.winLens focus => M.side)
        M.SideLog
        model

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
        Anno.CommentAttr x -> M.setEventAttr M.eventComment nodeId focus x model
        -- _ -> Debug.crash "SetEventAttr: not implemented yet!"

    CommandStart -> idle {model | command = Just ""}

    CommandChar c -> idle <|
      case model.command of
        Nothing -> model
        Just cmd -> {model | command = Just <| String.append cmd (String.fromChar c)}

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

--     SetEventClass nodeId focus x -> idle <|
--       M.setEventAttr M.eventClass nodeId focus x model

--     SetEventType nodeId focus x -> idle <|
--       M.setEventAttr M.eventType nodeId focus x model

--     SetEventTime nodeId focus x -> idle <|
--       M.setEventAttr M.eventTime nodeId focus x model

--     SetEventAspect nodeId focus x -> idle <|
--       M.setEventAttr M.eventAspect nodeId focus x model

--     -- Testing websockets
--     TestInput x -> idle <| {model | testInput=x}
--     TestGet x -> idle <| case Decode.decodeString M.fileDecoder x of
--       Ok ts -> M.setTrees ts model
--       Err err -> {model | testInput=err}
--     TestSend ->
--       ( {model | testInput=""}
--       , WebSocket.send Cfg.socketServer model.testInput )


    Many ms ->
      let f msg (mdl0, cmds) =
        let (mdl, cmd) = update msg mdl0
        in  (mdl, cmd :: cmds)
      in
        let (mdl, cmds) = L.foldl f (model, []) ms
        in  (mdl, Cmd.batch cmds)


-- | A dummy message.  Should avoid this...
dummy : Msg
dummy = Many []


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
  , ("parse", ParseSent Server.Stanford)
  , ("parsepos", ParseSentPos Server.Stanford)
  , ("dopparse", ParseSent Server.DiscoDOP)
  , ("dopparsepos", ParseSentPos Server.DiscoDOP)
  , ("parsecons", ParseSentCons)
  , ("flatten", ApplyRules)
  , ("concat", ConcatWords)
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
getWordPos : R.Tree M.Node -> List (Orth, Pos)
getWordPos tree =
  let
    go pos xs = case xs of
      [] -> []
      node :: nodes -> case node of
        M.Leaf r -> (r.nodeVal, pos) :: go "" nodes
        M.Node r -> go r.nodeVal nodes
  in
    go "" <| List.reverse <| R.flatten tree


----------------------------------------------
-- Utils
----------------------------------------------


parseSent : Server.ParserTyp -> M.Model -> (M.Model, Cmd Msg)
parseSent parTyp model =
    let
        treeId = (M.selectWin model.focus model).tree
        tree = M.getTree treeId model
        word node = case node of
                        M.Node _ -> Nothing
                        M.Leaf {nodeVal} -> Just nodeVal
        words = List.reverse <| Util.catMaybes <| List.map word <| R.flatten tree
        req = Server.encodeReq (Server.ParseSent model.fileId treeId parTyp words)
        send = WebSocket.send Cfg.socketServer req
    in
        (model, send)


-- | Retrieve the span of a given node in a given tree.
getSpan1 : M.NodeId -> R.Tree M.Node -> Maybe (Int, Int)
getSpan1 nodeId tree =
    case getSpan (S.singleton nodeId) tree of
        x :: _ -> Just x
        [] -> Nothing


-- | Retrieve the span of a given node in a given tree.
getSpan : S.Set M.NodeId -> R.Tree M.Node -> List (Int, Int)
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
