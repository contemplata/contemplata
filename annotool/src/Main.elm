-- | The main module of the Contemplata front-end application.


import Html as Html
import Mouse exposing (Position)
import Set as S
import Dict as D
import String as String
import Task as Task

import WebSocket
import Navigation
import Json.Decode as Decode

import Rose as R
import Config as Cfg

import Server
import Entry
import Edit.Core
import Edit.Config as AnnoCfg
import Edit.Model
import Edit.Init
import Edit.Message
import Edit.Message.Core
import Edit.View
import Edit.Subs
import Edit.Compare
import Edit.Popup as Popup


---------------------------------------------------
-- Switch
---------------------------------------------------


-- | The application can be in either of the two states: the entry page, or the
-- actually file editing mode.
type Switch a b
  = Edit a
    -- ^ File editing state (see the `Edit` subdirectory)
  | Entry b
    -- ^ Entry page (see the `Entry.elm` file)


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


getEntry : Switch a b -> Maybe b
getEntry top =
  case top of
    Entry x -> Just x
    _ -> Nothing


setEntry : b -> Switch a b -> Switch a b
setEntry mod top =
  case top of
    Entry _ -> Entry mod
    _ -> top


entryLens : MayLens (Switch a b) b
entryLens = (getEntry, setEntry)


---------------------------------------------------
-- Main
---------------------------------------------------


type alias Flags =
    { userName : String
      -- ^ Annotator's name
    , fileIds : List String
      -- ^ The IDs of the files to annotated
    , websocketServer : String
      -- ^ The main WebSocket server (for realtime communication with the
      -- backend)
    , websocketServerAlt : String
      -- ^ The alternative WebSocket server (OBSLETE)
    }


-- | The application's entry point.
main : Program Flags TopModel TopMsg
main =
  Html.programWithFlags
    { init = topInit
    , view = topView
    , update = topUpdate
    , subscriptions = topSubscriptions
    }


---------------------------------------------------
-- Top
---------------------------------------------------


-- | Top-level model.
type alias TopModel = Switch Edit.Model.Model Entry.Model


-- | Top- or sub-level message.
type alias TopMsg = Either (Switch Edit.Message.Core.Msg Entry.Msg) Msg


-- | Top-level message.
type Msg
  = ServerMsg Server.Answer
    -- ^ An answer received via WebSocket
  | Error String
    -- ^ An error message


-- | Make a top-level message from an edit message.
editMsg : Edit.Message.Core.Msg -> TopMsg
editMsg = Left << Edit


-- | Make a top-level message from an entry message.
entryMsg : Entry.Msg -> TopMsg
entryMsg = Left << Entry


-- | Make a top-level message from a top message.
topMsg : Msg -> TopMsg
topMsg = Right


-- | Get the configuration.
topCfg : TopModel -> Cfg.Config
topCfg top = case top of
  Edit mod -> mod.config
  Entry mod -> mod.config


-- | Get the annotation configuration.
topAnnoCfg : TopModel -> Maybe AnnoCfg.Config
topAnnoCfg top = case top of
  Edit mod -> Just mod.annoConfig
  Entry mod -> mod.annoConfig


---------------------------------------------------
-- View
---------------------------------------------------


-- | Top view method: selects the view of the currect (entry or edit) mode.
topView : TopModel -> Html.Html TopMsg
topView top = case top of
  Edit mod -> Html.map editMsg <| Edit.View.view mod
  Entry mod -> Html.map entryMsg <| Entry.view mod


---------------------------------------------------
-- Update
---------------------------------------------------


-- | Top message handler.
topUpdate : TopMsg -> TopModel -> ( TopModel, Cmd TopMsg )
topUpdate topMsg =
  case topMsg of

    -- Some editing mode messages need to be handled at the top-level.
    Left (Edit msg) -> case msg of
      -- Exit the frontend application and go to the main menu (it's the Snap
      -- server which is responsible for the main menu).
      Edit.Message.Core.Files -> \model -> (model, Navigation.load ".")
      -- Not pretty, but we need to handle the `Many` message in case there are
      -- some messages to be handled at the top-leve inside.
      Edit.Message.Core.Many msgs -> \model ->
        let f msg (mdl0, cmds) =
          let (mdl, cmd) = topUpdate (Left <| Edit msg) mdl0
          in  (mdl, cmd :: cmds)
        in
          let (mdl, cmds) = List.foldl f (model, []) msgs
          in  (mdl, Cmd.batch cmds)
      -- All the remaining messages are handled by the editing-specific handler.
      _ -> updateOn editLens editMsg (Edit.Message.update msg)

    -- All the entry-related messages are pushed downstream to the entry's
    -- message handler.
    Left (Entry msg) ->
      updateOn entryLens entryMsg (Entry.update msg)

    -- Top-level WebSocket messages, delegated to one of the downstream handlers
    -- (depending on the particular message).
    Right (ServerMsg ans) -> case ans of
      Server.Config annoCfg ->
        let upd = Entry.setAnnoConfig annoCfg
        in  updateOn entryLens entryMsg upd
      Server.NewFile fileId file -> \model_ ->
        case topAnnoCfg model_ of
          Nothing -> Debug.crash "Server.NewFile: annoConfig not set!"
          Just annoCfg ->
              let (edit, cmd) = Edit.Init.mkEdit (topCfg model_) annoCfg [(fileId, file)]
              in  (Edit edit, Cmd.map editMsg cmd)
      Server.NewFiles fileList -> \model_ ->
        case topAnnoCfg model_ of
          Nothing -> Debug.crash "Server.NewFile: annoConfig not set!"
          Just annoCfg ->
              let (edit, cmd) = Edit.Init.mkEdit (topCfg model_) annoCfg fileList
              in  (Edit edit, Cmd.map editMsg cmd)
      Server.DiffFiles fileIds ->
        let task = Task.succeed (Edit.Message.Core.Popup (Popup.Files (Just fileIds)) Nothing)
            upd model =
                ( model
                , Task.perform identity task )
        in  updateOn editLens editMsg upd
      Server.ParseResult fileId treeId sentMay tree ->
        let updTree = Edit.Model.setTreeCheck fileId treeId tree
            updSent = case sentMay of
                          Nothing -> identity
                          Just sent -> Edit.Model.setSentCheck fileId treeId sent
            upd model = (updSent <| updTree model, Cmd.none)
        in  updateOn editLens editMsg upd
      Server.ParseResultList fileId treeId mayForest ->
        let updTree = Edit.Model.setForestCheck fileId treeId mayForest
            upd model = (updTree model, Cmd.none)
        in  updateOn editLens editMsg upd
      Server.Notification msg ->
        let log model = (Edit.Model.log msg model, Cmd.none)
        in  updateOn editLens editMsg log
    Right (Error err) -> Debug.crash err
--     Right (Error err) -> \model -> case model of
--       Entry _ -> Debug.crash err
--       Edit _ -> updateOn
--         editLens editMsg
--         (Edit.Message.update msg) model


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


-- | At the top-level, Contemplata subscribes for WebSocket messages and for the
-- subordinate subscriptions (i.e., depending on the current application mode).
topSubscriptions : TopModel -> Sub TopMsg
topSubscriptions top =
  let
    getMsg x = case Decode.decodeString Server.answerDecoder x of
      Ok res -> ServerMsg res
      Err err -> Error err
    cfg = case top of
      Edit mod -> mod.config
      Entry mod -> mod.config
    listen = Server.listenWS cfg getMsg
    subordinate = case top of
      Edit mod -> Sub.map editMsg <| Edit.Subs.editSubscriptions mod
      Entry mod -> Sub.map entryMsg <| Entry.subscriptions mod
  in
    Sub.batch [Sub.map topMsg listen, subordinate]


---------------------------------------------------
-- Initialization
---------------------------------------------------


topInit : Flags -> (TopModel, Cmd TopMsg)
topInit r =
  let cfg =
          { user=r.userName
          , wsUseProxy=True
          , socketServer=r.websocketServer
          , socketServerAlt=r.websocketServerAlt }
      (model, cmd) =
          Entry.mkEntry cfg (List.filterMap Edit.Core.decodeFileId r.fileIds)
  in  (Entry model, Cmd.map entryMsg cmd)


---------------------------------------------
-- Utils
---------------------------------------------------


-- | The standard Either data type.
type Either a b
  = Left a
  | Right b


-- | Custom lenses
type alias MayLens big small =
  ( big -> Maybe small
    -- ^ Get maybe
  , small -> big -> big
    -- ^ Set
  )


-- | Perform update over the given element of the model.
updateOn
   : MayLens big small
     -- ^ Lens which indicates the element that is updated
  -> (smallMsg -> bigMsg)
     -- ^ A function which translates the messages corresponding to the element
     -- to the messages corresponding to the top-level model
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
