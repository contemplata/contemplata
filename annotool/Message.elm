module Message exposing (Msg(..), update, dummy)


import List as L
import Mouse exposing (Position)
import Task as Task
import Dom as Dom
import Model as M
import Config as Cfg


type Msg
    = DragStart M.Window Position
    | DragAt Position
    | DragEnd Position
    | Select M.Window M.NodeId
    | Focus M.Window
    | Resize Int -- ^ The height of the entire window
    | Increase Bool -- ^ Increase the size of the top window
    | Previous
    | Next
    | ChangeLabel M.NodeId M.Window String
    | EditLabel
    | Delete -- ^ Delete the selected nodes in the focused window
    | Add -- ^ Delete the selected nodes in the focused window
    | Many (List Msg)


-- update : Msg -> M.Model -> ( M.Model, Cmd Msg )
-- update msg model =
--   ( updateHelp msg model, Cmd.none )


update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =

 let idle x = (x, Cmd.none)

 in
  case msg of

    DragStart win xy -> idle <|
      { model | drag = Just (win, M.Drag xy xy) }

    DragAt xy -> idle <|
      { model
          | drag = Maybe.map (\(win, {start}) -> (win, M.Drag start xy)) model.drag
      }

    DragEnd _ -> idle <|
      -- { model | drag = Nothing, position = M.getPosition model }
      { model
          | drag = Nothing
          , topPos = case model.drag of
              Just (M.Top, _) -> M.getPosition M.Top model
              _ -> model.topPos
          , botPos = case model.drag of
              Just (M.Bot, _) -> M.getPosition M.Bot model
              _ -> model.botPos
      }

    Focus win -> idle <| {model | focus = win}

    Resize height -> idle <| {model | winHeight = height}

    Increase flag -> idle <|
      let
        newProp = trim <| model.winProp + change
        trim x = max 0 <| min 100 <| x
        change = case flag of
          True  -> Cfg.increaseSpeed
          False -> -Cfg.increaseSpeed
      in
        {model | winProp = newProp}

    Select win i -> idle <| M.select win i model
--     KeyDown key -> case key of
--       33 -> { model | focus = M.Top } -- Previous
--       34 -> { model | focus = M.Bot } -- Next
--       _  -> { model | focus = M.Top }

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
