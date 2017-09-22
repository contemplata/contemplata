module Server exposing (ParserTyp(..), Request(..), Answer(..), answerDecoder, encodeReq)


import Rose as R

-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode

import Edit.Model as M


---------------------------------------------------
-- Server websocket communication
---------------------------------------------------


type alias Orth = String
type alias Pos = String

-- | The type of parser to use.
type ParserTyp
  = Stanford
  | DiscoDOP


type Request
  = GetFiles
    -- ^ Obtain the list of files
  | GetFile M.FileId
    -- ^ Request the contents of the given file
  | SaveFile M.FileId M.File
    -- ^ Request the contents of the given file
  | ParseSent M.FileId M.TreeId ParserTyp (List Orth)
    -- ^ Parse the given list of words (the IDs are sent so that it can be
    -- checked on return if the user did not switch the file...)
  | ParseSentPos M.FileId M.TreeId ParserTyp (List (Orth, Pos))
    -- ^ Like `ParseSent`, but with POS tags
  | ParseSentCons M.FileId M.TreeId ParserTyp (List (Int, Int)) (List (Orth, Pos))
    -- ^ Like `ParseSent`, but with constraints


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
  ParseSent fileId treeId parTyp ws -> Encode.object
    [ ("tag", Encode.string "ParseSent")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int treeId
         , Encode.string (toString parTyp)
         , Encode.list (List.map Encode.string ws) ]
      )
    ]
  ParseSentPos fileId treeId parTyp ws -> Encode.object
    [ ("tag", Encode.string "ParseSentPos")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int treeId
         , Encode.string (toString parTyp)
         , Encode.list (List.map (encodePair Encode.string) ws) ]
      )
    ]
  ParseSentCons fileId treeId parTyp cons ws -> Encode.object
    [ ("tag", Encode.string "ParseSentCons")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int treeId
         , Encode.string (toString parTyp)
         , Encode.list (List.map (encodePair Encode.int) cons)
         , Encode.list (List.map (encodePair Encode.string) ws) ]
      )
    ]


type Answer
  = Files (List M.FileId)
    -- ^ The list of files
  | NewFile M.FileId M.File
    -- ^ New file to edit
  | ParseResult M.FileId M.TreeId (R.Tree M.Node)
    -- ^ New file to edit
  | Notification String
    -- ^ Just a notification message from a server


answerDecoder : Decode.Decoder Answer
answerDecoder = Decode.oneOf [filesDecoder, newFileDecoder, parseResultDecoder, notificationDecoder]


filesDecoder : Decode.Decoder Answer
filesDecoder =
  Decode.map Files
    (Decode.field "files" <| Decode.list Decode.string)


newFileDecoder : Decode.Decoder Answer
newFileDecoder =
  Decode.map2 NewFile
    (Decode.field "fileId" Decode.string)
    (Decode.field "file" M.fileDecoder)


parseResultDecoder : Decode.Decoder Answer
parseResultDecoder =
  Decode.map3 ParseResult
    (Decode.field "fileId" Decode.string)
    (Decode.field "treeId" Decode.int)
    (Decode.field "tree" M.treeDecoder)


notificationDecoder : Decode.Decoder Answer
notificationDecoder =
  Decode.map Notification
    (Decode.field "notification" <| Decode.string)


---------------------------------------------------
-- Utils
---------------------------------------------------


-- encodePair : (String, String) -> Encode.Value
-- encodePair (x, y) = Encode.list [Encode.string x, Encode.string y]


encodePair : (a -> Encode.Value) -> (a, a) -> Encode.Value
encodePair enc (x, y) = Encode.list [enc x, enc y]
