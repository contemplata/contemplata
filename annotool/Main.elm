-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
import Task as Task
-- import Dom as Dom
import Window as Window
import Mouse exposing (Position)
-- import List as L
import Set as S
import Dict as D
import String as String

import Rose as R
import Model as M
import Message as Msg
import Message exposing (Msg(..))
import View as V
import Config as Cfg


---------------------------------------------------
-- Main
---------------------------------------------------


main : Program Never M.Model Msg
main =
  Html.program
    { init = init
    , view = V.view
    , update = Msg.update
    , subscriptions = subscriptions
    }


---------------------------------------------------
-- Model
---------------------------------------------------


init : ( M.Model, Cmd Msg )
init =
  let
    top =
      { tree = "t1"
      , pos = Position 400 50
      , select = S.empty
      , drag = Nothing
      }
    bot =
      { tree = "t2"
      , pos = Position 400 50
      , select = S.empty
      , drag = Nothing
      }
    model =
      { trees = D.fromList
          [ ("t1", Cfg.testTree3)
          , ("t2", Cfg.testTree2)
          , ("t3", Cfg.testTree1)
          , ("t4", Cfg.testTree4)
          , ("t5", Cfg.testTree5)
          ]
      , top = top
      , bot = bot
      , focus = M.Top
      , links = S.fromList [(("t4", 3), ("t5", 9))]
      , winHeight = 0
      , winProp = 50
      }
    initHeight = Task.perform Resize Window.height
  in
    -- (model, Cmd.none)
    (model, initHeight)


---------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : M.Model -> Sub Msg
subscriptions model =
  let
    resize = Window.resizes (\x -> Resize x.height)
    win = M.selectWin model.focus model
  in
    case win.drag of
      Nothing ->
        Sub.batch [resize]
      Just _ ->
        Sub.batch
          [ resize
          , Mouse.moves DragAt
          , Mouse.ups DragEnd ]
