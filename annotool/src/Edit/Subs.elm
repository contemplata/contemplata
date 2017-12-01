module Edit.Subs exposing (editSubscriptions)


import Window as Window
import Mouse exposing (Position)
-- import WebSocket

import Focus exposing ((=>))
import Focus as Lens

-- import Config as Cfg
import Edit.Message.Core exposing (Msg(..))
import Edit.Model as Mod


editSubscriptions : Mod.Model -> Sub Msg
editSubscriptions model =
  let
    resize = Window.resizes Resize
    -- win = Edit.Model.selectWin model.focus model
    workspace = Lens.get (Mod.workspaceLens model.focus) model
  in
    case workspace.drag of
      Nothing ->
        Sub.batch [resize] --, listen]
      Just _ ->
        Sub.batch
          [ resize --, listen
          , Mouse.moves DragAt
          , Mouse.ups DragEnd ]
