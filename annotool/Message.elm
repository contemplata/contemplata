module Message exposing (Msg(..), update)


import Mouse exposing (Position)
import Model as M


type Msg
    = DragStart M.Window Position
    | DragAt Position
    | DragEnd Position
    | Select M.Window M.NodeId
    | Focus M.Window
    | Previous
    | Next
    | ChangeLabel M.NodeId M.Window String
    | Dummy
    | Delete -- ^ Delete the selected nodes in the focused window
    | Add -- ^ Delete the selected nodes in the focused window


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
    Select win i -> M.select win i model
    Focus win -> { model | focus = win }
--     KeyDown key -> case key of
--       33 -> { model | focus = M.Top } -- Previous
--       34 -> { model | focus = M.Bot } -- Next
--       _  -> { model | focus = M.Top }
    Next -> M.moveCursor True model
    Previous -> M.moveCursor False model
    ChangeLabel nodeId win newLabel -> M.setLabel nodeId win newLabel model
    Delete -> M.deleteSel model.focus model
    Add -> M.addSel model.focus model
    Dummy -> model -- it would be better to avoid this...
