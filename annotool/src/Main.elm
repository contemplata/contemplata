-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
-- import Dom as Dom
import Mouse exposing (Position)
-- import List as L
import Set as S
import Dict as D
import String as String
-- import Focus exposing ((=>))
-- import Focus as Lens

import WebSocket
import Json.Decode as Decode

import Rose as R
import Config as Cfg

import Menu

import Edit.Model
import Edit.Init
import Edit.Message
import Edit.View
import Edit.Subs


---------------------------------------------------
-- Switch
---------------------------------------------------


type Switch a b
  = Edit a
  | Menu b


getEdit : Switch a b -> Maybe a
getEdit top =
  case top of
    Edit x -> Just x
    _ -> Nothing


setEdit : a -> Switch a b -> Switch a b
setEdit mod top =
  case top of
    Edit _ -> Edit mod
    _ -> top


editLens : MayLens (Switch a b) a
editLens = (getEdit, setEdit)


getMenu : Switch a b -> Maybe b
getMenu top =
  case top of
    Menu x -> Just x
    _ -> Nothing


setMenu : b -> Switch a b -> Switch a b
setMenu mod top =
  case top of
    Menu _ -> Menu mod
    _ -> top


menuLens : MayLens (Switch a b) b
menuLens = (getMenu, setMenu)


---------------------------------------------------
-- Main
---------------------------------------------------


main : Program Never TopModel TopMsg
main =
  Html.program
    { init = topInit
    , view = topView
    , update = topUpdate
    , subscriptions = topSubscriptions
    }


---------------------------------------------------
-- Top
---------------------------------------------------


-- -- | Top-level model.
-- type TopModel
--   = Edit Edit.Model.Model
--   | Menu Menu.Menu


-- | Top-level model.
type alias TopModel = Switch Edit.Model.Model Menu.Model


-- | Top-level message.
type alias TopMsg = Switch Edit.Message.Msg Menu.Msg


---------------------------------------------------
-- View
---------------------------------------------------


topView : TopModel -> Html.Html TopMsg
topView top = case top of
  Edit mod -> Html.map Edit <| Edit.View.view mod
  Menu mod -> Html.map Menu <| Menu.view mod


---------------------------------------------------
-- Update
---------------------------------------------------


topUpdate : TopMsg -> TopModel -> ( TopModel, Cmd TopMsg )
topUpdate topMsg =
  case topMsg of
    Edit msg -> case msg of
      Edit.Message.Files -> \model_ ->
        let (model, cmd) = Menu.mkMenu
        in  (Menu model, Cmd.map Menu cmd)
      _ -> updateOn editLens Edit (Edit.Message.update msg)
    Menu msg -> case msg of
      Menu.ServerMsg (Menu.NewFile fileId file) -> \model_ ->
        let (edit, cmd) = Edit.Init.mkEdit fileId file
        in  (Edit edit, Cmd.map Edit cmd)
      _ -> updateOn menuLens Menu (Menu.update msg)


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


topSubscriptions : TopModel -> Sub TopMsg
topSubscriptions top =
  let
    getMsg x = case Decode.decodeString Menu.answerDecoder x of
      Ok res -> Menu.ServerMsg res
      Err err -> Menu.Error err
    listen = WebSocket.listen Cfg.socketServer getMsg
    subordinate = case top of
      Edit mod -> Sub.map Edit <| Edit.Subs.editSubscriptions mod
      Menu mod -> Sub.map Menu <| Menu.subscriptions mod
  in
    Sub.batch [Sub.map Menu listen, subordinate]


---------------------------------------------------
-- Initialization
---------------------------------------------------


topInit : (TopModel, Cmd TopMsg)
topInit =
--   let (edit, cmd) = editInit
--   in  (Edit edit, Cmd.map Edit cmd)
  let (model, cmd) = Menu.mkMenu
  in  (Menu model, Cmd.map Menu cmd)


-- editInit : (Edit.Model.Model, Cmd Edit.Message.Msg)
-- editInit =
--   let
--     top = win "t1"
--     bot = win "t2"
--     win name =
--       { tree = name
--       , pos = Position 400 50
--       , selMain = Nothing
--       , selAux = S.empty
--       , drag = Nothing
--       }
--     dim =
--       { width = 0
--       , height = 0
--       , heightProp = 50
--       }
--     model =
--       { trees = D.fromList
--           [ ("t1", Cfg.testTree3)
--           , ("t2", Cfg.testTree2)
--           , ("t3", Cfg.testTree1)
--           , ("t4", Cfg.testTree4)
--           , ("t5", Cfg.testTree5)
--           ]
--       , top = top
--       , bot = bot
--       , focus = Edit.Model.Top
--       , links = S.fromList
--           [ (("t4", 3), ("t5", 9))
--           , (("t1", 1), ("t1", 2))
--           ]
--       , dim = dim
--       , ctrl = False
--       , testInput = ""
--       }
--     initHeight = Task.perform Edit.Message.Resize Window.size
--   in
--     -- (model, Cmd.none)
--     (model, initHeight)


---------------------------------------------
-- Utils
---------------------------------------------------


-- | Perform update over the given element of the model.
updateOn
   : MayLens big small
     -- ^ Lens which indicates the element that is updated
  -> (smallMsg -> bigMsg)
     -- ^ A function which translates the messages corresponding to the element
     -- to the messages corresponding to the model
  -> (small -> (small, Cmd smallMsg))
     -- ^ The update function corresponding to the element
  -> (big -> (big, Cmd bigMsg))
     -- ^ The resulting update function
updateOn (get, set) bigCmd upd = \big ->
  case get big of
    Just small ->
      let (smallPrim, cmds) = upd small
      in  (set smallPrim big, Cmd.map bigCmd cmds)
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


---------------------------------------------------
-- Custom lenses
---------------------------------------------------


type alias MayLens big small =
  ( big -> Maybe small
    -- ^ Get maybe
  , small -> big -> big
    -- ^ Set
  )
