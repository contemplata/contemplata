module Message exposing (Msg(..), update, dummy)


import List as L
import Mouse exposing (Position)
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
    | Delete -- ^ Delete the selected nodes in the focused window
    | Add -- ^ Delete the selected nodes in the focused window
    | Many (List Msg)


update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> M.Model -> M.Model
updateHelp msg model =
  case msg of

    DragStart win xy ->
      { model | drag = Just (win, M.Drag xy xy) }

    DragAt xy ->
      { model
          | drag = Maybe.map (\(win, {start}) -> (win, M.Drag start xy)) model.drag
      }

    DragEnd _ ->
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

    Focus win -> {model | focus = win}

    Resize height -> {model | winHeight = height}

    Increase flag ->
      let
        newProp = trim <| model.winProp + change
        trim x = max 0 <| min 100 <| x
        change = case flag of
          True  -> Cfg.increaseSpeed
          False -> -Cfg.increaseSpeed
      in
        {model | winProp = newProp}

    Select win i -> M.select win i model
--     KeyDown key -> case key of
--       33 -> { model | focus = M.Top } -- Previous
--       34 -> { model | focus = M.Bot } -- Next
--       _  -> { model | focus = M.Top }

    Next -> M.moveCursor True model
    Previous -> M.moveCursor False model
    ChangeLabel nodeId win newLabel -> M.setLabel nodeId win newLabel model
    Delete -> M.deleteSel model.focus model
    Add -> M.addSel model.focus model
    Many ms -> L.foldl updateHelp model ms


-- | A dummy message.  Should avoid this...
dummy : Msg
dummy = Many []
