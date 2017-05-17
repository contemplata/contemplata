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
import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode
import Dict as D

import Util
import Config as Cfg
import Rose as R
import Edit.Model as M
import Server


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { fileIds : List M.FileId }


---------------------------------------------------
-- Messages
---------------------------------------------------


type Msg
  = Choice M.FileId -- ^ Edit a specific file
  | ShowFiles (List M.FileId)
--   | ServerMsg Answer -- ^ Get message from the websocket
--   | Error String  -- ^ An error message


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let idle x = (x, Cmd.none) in
  case msg of
    Choice fileId ->
      let cmd = WebSocket.send Cfg.socketServer <|
                  Server.encodeReq (Server.GetFile fileId)
      in  (model, cmd)
    ShowFiles ids -> idle <| {model | fileIds = ids}


---------------------------------------------------
-- View
---------------------------------------------------


view : Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "width" => "100%"
        , "height" => "100%"
        ]
    ]
    [ Html.ul []
        (List.map viewFileId model.fileIds)
    ]


viewFileId : M.FileId -> Html.Html Msg
viewFileId x = Html.li [] <| Util.single <|
  Html.div
    [ Atts.class "noselect"
    , Events.onClick (Choice x)
    , Atts.style ["cursor" => "pointer"]
    ]
    [Html.text x]


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkMenu : (Model, Cmd Msg)
mkMenu =
  let
    model = {fileIds = []}
    init = WebSocket.send Cfg.socketServer (Server.encodeReq Server.GetFiles)
  in
    (model, init)


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)
