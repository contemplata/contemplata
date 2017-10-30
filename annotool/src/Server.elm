module Server exposing
    ( ParserTyp(..), Request(..), Answer(..),  ParseReq(..)
    , answerDecoder, encodeReq, sendWS, listenWS
    )


import Rose as R

-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocket

import Edit.Model as M
import Edit.Core as C
import Config as Cfg


---------------------------------------------------
-- Server websocket communication
---------------------------------------------------


type alias Orth = String
type alias Pos = String

-- | The type of parser to use.
type ParserTyp
  = Stanford
  | DiscoDOP


type ParseReq a
    = Single a
    | Batch (List a)


encodeParseReq : (a -> Encode.Value) -> ParseReq a -> Encode.Value
encodeParseReq encA parseReq =
    case parseReq of
        Single x ->
            Encode.object
                [ ("tag", Encode.string "Single")
                , ("contents", encA x)
                ]
        Batch xs ->
            Encode.object
                [ ("tag", Encode.string "Batch")
                , ("contents", Encode.list (List.map encA xs))
                ]


type Request
  = GetFiles C.AnnoName
    -- ^ Obtain the list of files for a given annotator
  | GetFile C.AnnoName C.FileId
    -- ^ Request the contents of the given file
  | SaveFile C.AnnoName C.FileId M.File
    -- ^ Request the contents of the given file
  | Break C.FileId C.PartId (List String)
    -- ^ Break the given partition into its components
  | ParseRaw C.FileId C.PartId String Bool
    -- ^ Parse the given raw text
    -- QUESTION: do we really need to pass the string? After all, it should be
    -- on the server side as well...
    -- ANSWER: yes, we cannot be sure that the partition on the server is the same!
  | ParseSent C.FileId C.PartId ParserTyp (ParseReq (List Orth))
    -- ^ Parse the given list of words (the IDs are sent so that it can be
    -- checked on return if the user did not switch the file...)
  | ParseSentPos C.FileId C.PartId ParserTyp (ParseReq (List (Orth, Pos)))
    -- ^ Like `ParseSent`, but with POS tags
  | ParseSentCons C.FileId C.PartId ParserTyp (List (Int, Int)) (List (Orth, Pos))
    -- ^ Like `ParseSent`, but with constraints


encodeReq : Request -> String
encodeReq = Encode.encode 0 << encodeReqToVal


encodeReqToVal : Request -> Encode.Value
encodeReqToVal req = case req of
  -- GetFiles -> Encode.string "getfiles"
  GetFiles annoName -> Encode.object
    [ ("tag", Encode.string "GetFiles")
    , ("contents", Encode.string annoName)
    ]
  GetFile annoName id -> Encode.object
    [ ("tag", Encode.string "GetFile")
    -- , ("contents", Encode.string id)
    , ("contents", Encode.list
         [ Encode.string annoName
         , Encode.string id ]
      )
    ]
  SaveFile annoName fileId file -> Encode.object
    [ ("tag", Encode.string "SaveFile")
    , ("contents", Encode.list
         [ Encode.string annoName
         , Encode.string fileId
         , M.encodeFile file ]
      )
    ]
  Break fileId partId txts -> Encode.object
    [ ("tag", Encode.string "Break")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int partId
         , Encode.list (List.map Encode.string txts) ]
      )
    ]
  ParseRaw fileId treeId txt prep -> Encode.object
    [ ("tag", Encode.string "ParseRaw")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int treeId
         , Encode.string txt
         , Encode.bool prep ]
      )
    ]
  ParseSent fileId treeId parTyp parseReq ->
    let encList ws = Encode.list (List.map Encode.string ws) in
    Encode.object
      [ ("tag", Encode.string "ParseSent")
      , ("contents", Encode.list
           [ Encode.string fileId
           , Encode.int treeId
           , Encode.string (toString parTyp)
           , encodeParseReq encList parseReq ]
        )
      ]
  ParseSentPos fileId treeId parTyp parseReq ->
    let encList ws = Encode.list (List.map (encodePair Encode.string) ws) in
    Encode.object
      [ ("tag", Encode.string "ParseSentPos")
      , ("contents", Encode.list
           [ Encode.string fileId
           , Encode.int treeId
           , Encode.string (toString parTyp)
           , encodeParseReq encList parseReq ]
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
  = Files (List C.FileId)
    -- ^ The list of files
  | NewFile C.FileId M.File
    -- ^ New file to edit
  | ParseResult C.FileId C.PartId (Maybe M.Sent) (R.Tree M.Node)
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
  Decode.map4 ParseResult
    (Decode.field "fileId" Decode.string)
    (Decode.field "treeId" Decode.int)
    (Decode.field "sent" (Decode.nullable M.sentDecoder))
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


-- | Send the message to the underlying websocket server.
sendWS : Cfg.Config -> Request -> Cmd msg
sendWS cfg req =
    let
        socketServer =
            if cfg.wsUseProxy
            then cfg.socketServer
            else cfg.socketServerAlt
    in
        WebSocket.send socketServer (encodeReq req)


-- | Send the message to the underlying websocket server.
-- listenWS : {record | wsUseProxy : Bool} -> Request -> Cmd msg
listenWS cfg =
    let
        socketServer =
            if cfg.wsUseProxy
            then cfg.socketServer
            else cfg.socketServerAlt
    in
        WebSocket.listen socketServer
