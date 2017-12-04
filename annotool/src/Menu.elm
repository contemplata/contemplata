module Menu exposing
  (
  -- Model
    Model
  -- Messages
  , Msg(..), update
  -- View
  , view
  -- Subscriptions
  , subscriptions
  -- Server communication
  -- , Request(..) , Answer (..), answerDecoder, encodeReq
  -- Initialization
  , mkMenu
  )


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Dict as D

import Util
import Config as Cfg
import Rose as R
import Edit.Core as C
import Server


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { config : Cfg.Config
  , message : String
  }


---------------------------------------------------
-- Messages
---------------------------------------------------


type Msg = Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)
--   let idle x = (x, Cmd.none) in
--   case msg of
--     Choice fileId ->
--       let cmd = Server.sendWS model.config (Server.GetFile model.config.user fileId)
--       in  (model, cmd)
--     ShowFiles ids -> idle <| {model | fileIds = ids}
--     SetProxy newProxy ->
--         let
--             oldConfig = model.config
--             newConfig = {oldConfig | wsUseProxy = newProxy}
--             newModel = {model | config = newConfig}
--             getFiles = Server.sendWS newConfig (Server.GetFiles newConfig.user)
--         in
--             (newModel, getFiles)
--     Many msgs ->
--       let f msg (mdl0, cmds) =
--         let (mdl, cmd) = update msg mdl0
--         in  (mdl, cmd :: cmds)
--       in
--         let (mdl, cmds) = List.foldl f (model, []) msgs
--         in  (mdl, Cmd.batch cmds)


---------------------------------------------------
-- View
---------------------------------------------------


view : Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "position" => "absolute"
        , "left" => "50%"
        , "top" => "50%"
        , "-webkit-transform" => "translate(-50%, -50%)"
        , "transform" => "translate(-50%, -50%)"
        ]
    ]
    [ Html.text model.message
    ]

---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkMenu
    : Cfg.Config
    -> List C.FileId
    -> (Model, Cmd Msg)
mkMenu config fileIds =
  let
    msg =
        case fileIds of
            [] -> "Incorrent file IDs"
            _  -> "Fetching " ++
                  String.join ", " (List.map C.encodeFileId fileIds)
    model = {config=config, message=msg}
    init =
        case fileIds of
            [] -> Cmd.none
            _ -> Server.sendWS config (Server.GetFiles config.user fileIds)
  in
    (model, init)


-- -- | For adjudication.
-- mkMenu2
--     : Cfg.Config
--     -> Maybe C.FileId
--     -> Maybe C.FileId
--     -> (Model, Cmd Msg)
-- mkMenu2 config fileIdMay compIdMay =
--   let
--     msg =
--         case (fileIdMay, compIdMay) of
--             (Just fileId, Just compId) ->
--                 "Fetching " ++ C.encodeFileId fileId ++
--                 " and " ++ C.encodeFileId compId
--             _ -> "Incorrent file ID(s)"
--     model = {config=config, message=msg}
--     init =
--         case (fileIdMay, compIdMay) of
--             (Just fileId, Just compId) ->
--                 Server.sendWS config
--                     (Server.GetFiles config.user [fileId, compId])
--             _ -> Cmd.none
--   in
--     (model, init)


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)
