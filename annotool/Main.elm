-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
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
      , topPos = Position 640 200
      , botPos = Position 640 200
      , drag = Nothing
      , focus = M.Top
      , topSelect = S.empty
      , botSelect = S.empty
      }
  in
    (model, Cmd.none)


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : M.Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
