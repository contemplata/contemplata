module Server exposing
    ( ParserTyp(..), Request(..), Answer(..),  ParseReq(..)
    , answerDecoder, encodeReq, sendWS, listenWS
    )


import Rose as R

-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocket

import Server.Core exposing (..)
import Edit.Model as M
import Edit.Core as C
import Edit.Config
import Config as Cfg
import Util as Util


---------------------------------------------------
-- Server websocket communication
---------------------------------------------------


type alias Orth = String
type alias Pos = String


type Request
  -- = GetFiles C.AnnoName
    -- ^ Obtain the list of files for a given annotator
  = GetFile C.AnnoName C.FileId
    -- ^ Request the contents of the given file
  | GetFiles C.AnnoName (List C.FileId)
    -- ^ Request several files for annotation
  | GetConfig
    -- ^ Request the annotation configuration
  | SaveFile C.AnnoName C.FileId M.File
    -- ^ Request the contents of the given file
  | CompareFiles (List (C.FileId, M.File))
    -- ^ Request the comparison of the given files
  | Break C.FileId C.PartId (List String)
    -- ^ Break the given partition into its components
  | ParseRaw C.FileId C.PartId String Bool
    -- ^ Parse the given raw text
    -- QUESTION: do we really need to pass the string? After all, it should be
    -- on the server side as well...
    -- ANSWER: yes, we cannot be sure that the partition on the server is the same!
  | ParseSent C.FileId C.PartId ParserTyp
    (List (Bool, List (M.Token, Maybe Orth)))
    -- ^ Parse the given list of words (the IDs are sent so that it can be
    -- checked on return if the user did not switch the file...)
  | ParseSentPos C.FileId C.PartId ParserTyp
    (List (Bool, List (M.Token, Maybe (Orth, Pos))))
    -- ^ A version of `ParseSentPos` which allows to *not* to reparse the entire
    -- tree, but only some of the SENT subtrees. The `Bool` argument, provided
    -- for each sub-sentence, tells whether it should be reparsed or not.
    --
    -- TODO: do we really need to know the tokens for the subsentences we are
    -- not going to parse?
  | ParseSentCons C.FileId C.PartId ParserTyp
    (ParseReq (List (String, Int, Int), List (M.Token, Maybe (Orth, Pos))))
    -- ^ Like `ParseSent`, but with POS tags and constraints


encodeReq : Request -> String
encodeReq = Encode.encode 0 << encodeReqToVal


encodeReqToVal : Request -> Encode.Value
encodeReqToVal req = case req of
--   GetFiles annoName -> Encode.object
--     [ ("tag", Encode.string "GetFiles")
--     , ("contents", Encode.string annoName)
--     ]
  GetFile annoName id -> Encode.object
    [ ("tag", Encode.string "GetFile")
    -- , ("contents", Encode.string id)
    , ("contents", Encode.list
         [ Encode.string annoName
         , C.encodeFileIdJSON id ]
      )
    ]
  GetFiles annoName ids -> Encode.object
    [ ("tag", Encode.string "GetFiles")
    , ("contents", Encode.list
         [ Encode.string annoName
         , Encode.list (List.map C.encodeFileIdJSON ids) ]
      )
    ]
  GetConfig -> Encode.object
    [ ("tag", Encode.string "GetConfig")
    ]
  SaveFile annoName fileId file -> Encode.object
    [ ("tag", Encode.string "SaveFile")
    , ("contents", Encode.list
         [ Encode.string annoName
         , C.encodeFileIdJSON fileId
         , M.encodeFile file ]
      )
    ]
  CompareFiles fileList ->
      let
          encodePair (fileId, file) = Encode.list
            [ C.encodeFileIdJSON fileId
            , M.encodeFile file ]
      in
        Encode.object
          [ ("tag", Encode.string "CompareFiles")
          , ("contents", Encode.list (List.map encodePair fileList))
          ]
  Break fileId partId txts -> Encode.object
    [ ("tag", Encode.string "Break")
    , ("contents", Encode.list
         [ C.encodeFileIdJSON fileId
         , Encode.int partId
         , Encode.list (List.map Encode.string txts) ]
      )
    ]
  ParseRaw fileId treeId txt prep -> Encode.object
    [ ("tag", Encode.string "ParseRaw")
    , ("contents", Encode.list
         [ C.encodeFileIdJSON fileId
         , Encode.int treeId
         , Encode.string txt
         , Encode.bool prep ]
      )
    ]

  ParseSent fileId treeId parTyp sents ->
    let
        encOrth orth =
            Encode.string orth
        encPair (tok, mayOrth) =
            Encode.list
                [ M.encodeToken tok
                , Util.encodeMaybe encOrth mayOrth ]
        encList ws = Encode.list (List.map encPair ws)
        encTop (parseIt, ws) =
            Encode.list
                [ (Encode.bool parseIt)
                , (encList ws) ]
    in
        Encode.object
          [ ("tag", Encode.string "ParseSent")
          , ("contents", Encode.list
               [ C.encodeFileIdJSON fileId
               , Encode.int treeId
               , Encode.string (toString parTyp)
               , Encode.list (List.map encTop sents) ]
            )
          ]

  ParseSentPos fileId treeId parTyp sents ->
    let
        encOrthPos (orth, pos) =
            Encode.list
                [ Encode.string orth
                , Encode.string pos ]
        encPair (tok, mayOrthPos) =
            Encode.list
                [ M.encodeToken tok
                , Util.encodeMaybe encOrthPos mayOrthPos ]
        encList ws = Encode.list (List.map encPair ws)
        encTop (parseIt, ws) =
            Encode.list
                [ (Encode.bool parseIt)
                , (encList ws) ]
    in
        Encode.object
          [ ("tag", Encode.string "ParseSentPos")
          , ("contents", Encode.list
               [ C.encodeFileIdJSON fileId
               , Encode.int treeId
               , Encode.string (toString parTyp)
               , Encode.list (List.map encTop sents) ]
            )
          ]

  ParseSentCons fileId treeId parTyp parseReq ->
    let
        encOrthPos (orth, pos) =
            Encode.list
                [ Encode.string orth
                , Encode.string pos ]
        encPair (tok, mayOrthPos) =
            Encode.list
                [ M.encodeToken tok
                , Util.encodeMaybe encOrthPos mayOrthPos ]
        encodeCons (label, p, q) =
            Encode.list
                [ Encode.string label
                , Encode.int p
                , Encode.int q ]
        encReq (cons, rest) =
            Encode.list
                [ Encode.list (List.map encodeCons cons)
                , Encode.list (List.map encPair rest) ]
    in
        Encode.object
          [ ("tag", Encode.string "ParseSentCons")
          , ("contents", Encode.list
               [ C.encodeFileIdJSON fileId
               , Encode.int treeId
               , Encode.string (toString parTyp)
               , encodeParseReq encReq parseReq ]
            )
          ]

--   ParseSentCons fileId treeId parTyp cons ws -> Encode.object
--     [ ("tag", Encode.string "ParseSentCons")
--     , ("contents", Encode.list
--          [ Encode.string fileId
--          , Encode.int treeId
--          , Encode.string (toString parTyp)
--          , Encode.list (List.map (encodePair Encode.int) cons)
--          , Encode.list (List.map (encodePair Encode.string) ws) ]
--       )
--     ]


type Answer
  = Files (List C.FileId)
    -- ^ The list of files
  | NewFile C.FileId M.File
    -- ^ New file to edit
--   | NewFile2 C.FileId M.File C.FileId M.File
--     -- ^ New pair of files to edit
  | NewFiles (List (C.FileId, M.File))
    -- ^ New list of files to annotate
  | Config Edit.Config.Config
    -- ^ Annotation config
  | ParseResult C.FileId C.PartId (Maybe M.Sent) (R.Tree M.Node)
    -- ^ Parsing result
  | ParseResultList C.FileId C.PartId (List (Maybe (R.Tree M.Node)))
    -- ^ Parsing result list
  | DiffFiles (List C.FileId)
    -- ^ Coparison result
  | Notification String
    -- ^ Just a notification message from a server


answerDecoder : Decode.Decoder Answer
answerDecoder =
    Decode.oneOf
        [ filesDecoder
        , newFileDecoder
        , newFilesDecoder
        , configDecoder
        , parseResultDecoder
        , parseResultListDecoder
        , diffFilesDecoder
        , notificationDecoder]


filesDecoder : Decode.Decoder Answer
filesDecoder =
  Decode.map Files
    (Decode.field "files" <| Decode.list C.fileIdDecoder)


newFileDecoder : Decode.Decoder Answer
newFileDecoder =
  Decode.map3 (\tag_ -> NewFile)
    (Decode.field "tag" <| tagDecoder "NewFile")
    (Decode.field "fileId" C.fileIdDecoder)
    (Decode.field "file" M.fileDecoder)


newFilesDecoder : Decode.Decoder Answer
newFilesDecoder =
  Decode.map2 (\tag_ -> NewFiles)
    (Decode.field "tag" <| tagDecoder "NewFiles")
    (Decode.field "files" fileListDecoder)


configDecoder : Decode.Decoder Answer
configDecoder =
  Decode.map2 (\tag_ -> Config)
    (Decode.field "tag" <| tagDecoder "Config")
    (Decode.field "config" Edit.Config.configDecoder)


fileListDecoder : Decode.Decoder (List (C.FileId, M.File))
fileListDecoder =
  let
      pairDecoder = Decode.map2 (\fileId file -> (fileId, file))
          (Decode.field "fileId" C.fileIdDecoder)
          (Decode.field "file" M.fileDecoder)
  in
      Decode.list pairDecoder


tagDecoder : String -> Decode.Decoder ()
tagDecoder tag =
    Decode.string |> Decode.andThen
        ( \str ->
              if str == tag
              then Decode.succeed ()
              else Decode.fail "Incorrect tag"
        )


parseResultDecoder : Decode.Decoder Answer
parseResultDecoder =
  Decode.map4 ParseResult
    (Decode.field "fileId" C.fileIdDecoder)
    (Decode.field "treeId" Decode.int)
    (Decode.field "sent" (Decode.nullable M.sentDecoder))
    (Decode.field "tree" M.treeDecoder)


parseResultListDecoder : Decode.Decoder Answer
parseResultListDecoder =
  Decode.map3 ParseResultList
    (Decode.field "fileId" C.fileIdDecoder)
    (Decode.field "treeId" Decode.int)
    (Decode.field "forest" (Decode.list <| Decode.nullable M.treeDecoder))


diffFilesDecoder : Decode.Decoder Answer
diffFilesDecoder =
  Decode.map DiffFiles
    (Decode.field "fileIds" (Decode.list C.fileIdDecoder))


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
