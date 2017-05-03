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
    model =
      { trees = D.fromList
          [ ("t1", Cfg.testTree3)
          , ("t2", Cfg.testTree2)
          , ("t3", Cfg.testTree1)
          ]
      , topTree = "t1"
      , botTree = "t2"
      , topPos = Position 400 50
      , botPos = Position 400 50
      , drag = Nothing
      , focus = M.Top
      , topSelect = S.empty
      , botSelect = S.empty
      , links = S.empty
      , winHeight = 0
      , winProp = 50
      }
    initHeight = Task.perform Resize Window.height
  in
    -- (model, Cmd.none)
    (model, initHeight)


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : M.Model -> Sub Msg
subscriptions model =
  let
    resize = Window.resizes (\x -> Resize x.height)
  in
    case model.drag of
      Nothing ->
        Sub.batch [resize]
      Just _ ->
        Sub.batch [resize, Mouse.moves DragAt, Mouse.ups DragEnd]
