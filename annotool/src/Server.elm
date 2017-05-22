module Server exposing (Request(..), Answer(..), answerDecoder, encodeReq)


-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode

import Edit.Model as M


---------------------------------------------------
-- Server websocket communication
---------------------------------------------------


type Request
  = GetFiles
    -- ^ Obtain the list of files
  | GetFile M.FileId
    -- ^ Request the contents of the given file
  | SaveFile M.FileId M.File
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
  SaveFile fileId file -> Encode.object
    [ ("tag", Encode.string "SaveFile")
    , ("contents", Encode.list
         [ Encode.string fileId
         , M.encodeFile file ]
      )
    ]


type Answer
  = Files (List M.FileId)
    -- ^ The list of files
  | NewFile M.FileId M.File
    -- ^ New file to edit
  | Notification String
    -- ^ Just a notification message from a server


answerDecoder : Decode.Decoder Answer
answerDecoder = Decode.oneOf [filesDecoder, newFileDecoder, notificationDecoder]


filesDecoder : Decode.Decoder Answer
filesDecoder =
  Decode.map Files
    (Decode.field "files" <| Decode.list Decode.string)


newFileDecoder : Decode.Decoder Answer
newFileDecoder =
  Decode.map2 NewFile
    (Decode.field "fileId" Decode.string)
    (Decode.field "file" M.fileDecoder)


notificationDecoder : Decode.Decoder Answer
notificationDecoder =
  Decode.map Notification
    (Decode.field "notification" <| Decode.string)


---------------------------------------------------
-- Utils
---------------------------------------------------
