module Edit.Subs exposing (editSubscriptions)


import Window as Window
import Mouse exposing (Position)
-- import WebSocket

-- import Config as Cfg
import Edit.Message exposing (Msg(..))
import Edit.Model


editSubscriptions : Edit.Model.Model -> Sub Msg
editSubscriptions model =
  let
    resize = Window.resizes Resize
    win = Edit.Model.selectWin model.focus model
    -- listen = WebSocket.listen Cfg.socketServer TestGet
  in
    case win.drag of
      Nothing ->
        Sub.batch [resize] --, listen]
      Just _ ->
        Sub.batch
          [ resize --, listen
          , Mouse.moves DragAt
          , Mouse.ups DragEnd ]
