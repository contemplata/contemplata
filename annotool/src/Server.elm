module Server exposing
    ( ParserTyp(..), Request(..), Answer(..),  ParseReq(..)
    , answerDecoder, encodeReq
    )


import Rose as R

-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode

import Edit.Model as M
import Edit.Core as C


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
  = GetFiles
    -- ^ Obtain the list of files
  | GetFile C.FileId
    -- ^ Request the contents of the given file
  | SaveFile C.FileId M.File
    -- ^ Request the contents of the given file
  | ParseRaw C.FileId C.TreeId String
    -- ^ Parse the given raw text
  | ParseSent C.FileId C.TreeId ParserTyp (ParseReq (List Orth))
    -- ^ Parse the given list of words (the IDs are sent so that it can be
    -- checked on return if the user did not switch the file...)
  | ParseSentPos C.FileId C.TreeId ParserTyp (ParseReq (List (Orth, Pos)))
    -- ^ Like `ParseSent`, but with POS tags
  | ParseSentCons C.FileId C.TreeId ParserTyp (List (Int, Int)) (List (Orth, Pos))
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
  ParseRaw fileId treeId txt -> Encode.object
    [ ("tag", Encode.string "ParseRaw")
    , ("contents", Encode.list
         [ Encode.string fileId
         , Encode.int treeId
         , Encode.string txt ]
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
  | ParseResult C.FileId C.TreeId (R.Tree M.Node)
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
