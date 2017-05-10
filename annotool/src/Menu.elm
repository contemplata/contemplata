module Menu exposing
  (
  -- Model
    Model, FileId
  -- Messages
  , Msg(..), update
  -- View
  , view
  -- Subscriptions
  , subscriptions
  -- Server communication
  , Request(..) , Answer (..), answerDecoder
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

import Config as Cfg
import Rose as R
import Edit.Model as M


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { fileIds : List FileId }


type alias FileId = String


---------------------------------------------------
-- Messages
---------------------------------------------------


type Msg
  = Choice FileId -- ^ Edit a specific file
  | ServerMsg Answer -- ^ Get message from the websocket
  | Error String  -- ^ An error message


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let idle x = (x, Cmd.none) in
  case msg of
    Choice fileId ->
      let cmd = WebSocket.send Cfg.socketServer <| encodeReq (GetFile fileId)
      in  (model, cmd)
    ServerMsg (Files ids) -> idle <| {model | fileIds = ids}
    _ -> idle <| model


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


viewFileId : FileId -> Html.Html Msg
viewFileId x = Html.li [] <| single <|
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
subscriptions _ = -- Sub.none
  let
    listen = WebSocket.listen Cfg.socketServer getMsg
    getMsg x = case Decode.decodeString answerDecoder x of
      Ok res -> ServerMsg res
      Err err ->  Error err
  in
    Sub.batch [listen]


---------------------------------------------------
-- WebSocket communication
---------------------------------------------------


type Request
  = GetFiles
    -- ^ Obtain the list of files
  | GetFile FileId
    -- ^ Request the contents of the given file


encodeReq : Request -> String
encodeReq = Encode.encode 0 << encodeReqToVal


encodeReqToVal : Request -> Encode.Value
encodeReqToVal req = case req of
  -- GetFiles -> Encode.string "getfiles"
  GetFiles -> Encode.object [("tag", Encode.string "GetFiles")]
  GetFile id -> Encode.object
    [ ("tag", Encode.string "GetFile")
    , ("contents", Encode.string id)
    ]


type Answer
  = Files (List FileId)
    -- ^ The list of files
  | NewFile FileId M.File
    -- ^ New file to edit


answerDecoder : Decode.Decoder Answer
answerDecoder = Decode.oneOf [filesDecoder, newFileDecoder]


filesDecoder : Decode.Decoder Answer
filesDecoder =
  Decode.map Files
    (Decode.field "files" <| Decode.list Decode.string)


newFileDecoder : Decode.Decoder Answer
newFileDecoder =
  Decode.map2 NewFile
    (Decode.field "fileId" Decode.string)
    (Decode.field "file" M.fileDecoder)


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkMenu : (Model, Cmd Msg)
mkMenu =
  let
    model = {fileIds = []}
    init = WebSocket.send Cfg.socketServer (encodeReq GetFiles)
  in
    (model, init)


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)


single : a -> List a
single x = [x]
