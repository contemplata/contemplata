module Menu exposing
  (
  -- Model
    Model
  , setAnnoConfig
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
import Edit.Config
import Rose as R
import Edit.Core as C
import Server


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { config : Cfg.Config
  , annoConfig : Maybe Edit.Config.Config
  , fileIds : List C.FileId
  }


-- | Set the annotation configuration, and, once this is done, set the request
-- to fetch the files.
setAnnoConfig : Edit.Config.Config -> Model -> (Model, Cmd Msg)
setAnnoConfig annoCfg model =
    ( {model | annoConfig = Just (Debug.log "annoCfg" annoCfg)}
    , Server.sendWS model.config (Server.GetFiles model.config.user model.fileIds)
    -- , Cmd.none
    )


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
    [ Html.text <|
          case model.fileIds of
              [] -> "Incorrent file IDs"
              _  -> case model.annoConfig of
                  Nothing  -> "Fetching annotation configuration"
                  Just cfg -> "Fetching " ++
                      String.join ", " (List.map C.encodeFileId model.fileIds)
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
    model = {config=config, annoConfig=Nothing, fileIds=fileIds}
    init =
        case fileIds of
            [] -> Cmd.none
            -- _ -> Server.sendWS config (Server.GetFiles config.user fileIds)
            _ -> Server.sendWS config Server.GetConfig
  in
    (model, init)


-- mkMenu
--     : Cfg.Config
--     -> List C.FileId
--     -> (Model, Cmd Msg)
-- mkMenu config fileIds =
--   let
--     msg =
--         case fileIds of
--             [] -> "Incorrent file IDs"
--             _  -> "Fetching " ++
--                   String.join ", " (List.map C.encodeFileId fileIds)
--     model = {config=config, annoConfig=Nothing, message=msg}
--     init =
--         case fileIds of
--             [] -> Cmd.none
--             _ -> Server.sendWS config (Server.GetFiles config.user fileIds)
--   in
--     (model, init)


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)
