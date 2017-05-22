module Edit.Message exposing (Msg(..), update, dummy)


import List as L
import Mouse exposing (Position)
import Task as Task
import Dom as Dom
import Focus exposing ((=>))
import Focus as Focus
import Window as Window
import WebSocket
import Json.Decode as Decode

import Config as Cfg
import Edit.Model as M
import Menu
import Server


type Msg
  = DragStart M.Focus Position
    -- ^ Neither `DragAt` nor `DragEnd` have their focus. This is on purpose.
    -- Focus should be determined, in their case, on the basis of the drag in
    -- the underlying model. We do not support concurrent drags at the moment.
  | DragAt Position
  | DragEnd Position
  | Select M.Focus M.NodeId
  | SelectTree M.Focus M.TreeId
  | Focus M.Focus
  | Resize Window.Size -- ^ The height and width of the entire window
  | Increase Bool Bool -- ^ Change the proportions of the window
  | Previous
  | Next
  | ChangeLabel M.NodeId M.Focus String
  | EditLabel
  | Delete -- ^ Delete the selected nodes in the focused window
  | Add -- ^ Delete the selected nodes in the focused window
  | CtrlDown
  | CtrlUp
  | Connect
  | Attach
  | Files -- ^ Go back to files menu
  | SaveFile  -- ^ Save the current file
  | SideMenuEdit M.Focus
  | SideMenuContext M.Focus
  | SideMenuLog M.Focus
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

 let idle x = (x, Cmd.none)

 in
  case msg of

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

    Next -> idle <| M.moveCursor True model

    Previous -> idle <| M.moveCursor False model

    ChangeLabel nodeId win newLabel -> idle <| M.setLabel nodeId win newLabel model

    EditLabel ->
      let target = case model.focus of
        M.Top -> Cfg.editLabelName True
        M.Bot -> Cfg.editLabelName False
      in
        ( model
        , Task.attempt
            (\_ -> dummy)
            (Dom.focus target)
        )

    Delete -> idle <| M.deleteSel model.focus model

    Add -> idle <| M.addSel model.focus model

    CtrlDown -> idle <| {model | ctrl=True}
    CtrlUp -> idle <| {model | ctrl=False}

    Connect -> idle <| M.connect model

    Attach -> idle <| M.attachSel model

    Files -> idle <| model -- ^ Handled upstream

    SaveFile ->
      let
        file = {treeMap = model.trees, linkSet = model.links}
        req = Server.encodeReq (Server.SaveFile model.fileId file)
        save = WebSocket.send Cfg.socketServer req
      in
        (model, save)

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
