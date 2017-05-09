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
import WebSocket
-- import Focus exposing ((=>))
-- import Focus as Lens

import Rose as R
import Config as Cfg
import Edit.Model
import Edit.Message as Msg
import Edit.Message exposing (Msg(..))
import Edit.View
import Menu


---------------------------------------------------
-- Main
---------------------------------------------------


main : Program Never TopModel Msg
main =
  Html.program
    { init = init
    , view = topView
    , update = updateOn modelLens Msg.update
    , subscriptions = topSubscriptions
    }


---------------------------------------------------
-- Model
---------------------------------------------------


-- | Top-level model.
type TopModel
  = Edit Edit.Model.Model
  | Menu Menu.Menu


type alias MayLens big small =
  ( big -> Maybe small
  , small -> big -> big )


getEditModel : TopModel -> Maybe Edit.Model.Model
getEditModel top =
  case top of
    Edit mod -> Just mod
    _ -> Nothing


setEditModel : Edit.Model.Model -> TopModel -> TopModel
setEditModel mod top =
  case top of
    Edit _ -> Edit mod
    _ -> top


modelLens : MayLens TopModel Edit.Model.Model
modelLens = (getEditModel, setEditModel)


init : ( TopModel, Cmd Msg )
init =
  let
    top = win "t1"
    bot = win "t2"
    win name =
      { tree = name
      , pos = Position 400 50
      , selMain = Nothing
      , selAux = S.empty
      , drag = Nothing
      }
    dim =
      { width = 0
      , height = 0
      , heightProp = 50
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
      , focus = Edit.Model.Top
      , links = S.fromList
          [ (("t4", 3), ("t5", 9))
          , (("t1", 1), ("t1", 2))
          ]
      , dim = dim
      , ctrl = False
      , testInput = ""
      }
    initHeight = Task.perform Resize Window.size
  in
    -- (model, Cmd.none)
    (Edit model, initHeight)


---------------------------------------------------
-- View
---------------------------------------------------


topView : TopModel -> Html.Html Msg
topView top = case top of
  Edit mod -> Edit.View.view mod
  Menu mod -> Html.div [] []


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Edit.Model.Model -> Sub Msg
subscriptions model =
  let
    resize = Window.resizes Resize
    win = Edit.Model.selectWin model.focus model
    listen = WebSocket.listen Cfg.socketServer TestGet
  in
    case win.drag of
      Nothing ->
        Sub.batch [resize, listen]
      Just _ ->
        Sub.batch
          [ resize, listen
          , Mouse.moves DragAt
          , Mouse.ups DragEnd ]


topSubscriptions : TopModel -> Sub Msg
topSubscriptions top = case top of
  Edit mod -> subscriptions mod
  _ -> Sub.batch []


---------------------------------------------
-- Utils
---------------------------------------------------


-- | Perform update over the given element of the model.
updateOn
  : MayLens big small
  -> (msg -> small -> (small, Cmd msg))
  -> (msg -> big -> (big, Cmd msg))
updateOn (get, set) upd = \msg big ->
  case get big of
    Just small ->
      let (smallPrim, cmds) = upd msg small
      in  (set smallPrim big, cmds)
    Nothing -> (big, Cmd.none)


-- -- | Perform update over the given element of the model.
-- updateOn
--   : Lens.Focus b a
--   -> (msg -> a -> (a, cmd))
--   -> (msg -> b -> (b, cmd))
-- updateOn lens upd = \msg big ->
--   let
--     small = Lens.get lens big
--     (smallPrim, cmds) = upd msg small
--   in
--     (Lens.set lens smallPrim big, cmds)
