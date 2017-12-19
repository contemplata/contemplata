-- | Core types

module Edit.Core exposing
  ( FileId
  , AnnoName
  , NodeId
  , Addr

  -- * Encoding
  , encodeFileId
  , decodeFileId
  -- , showAnnoLevel
  -- , readAnnoLevel

  , TreeId (..)
  , TreeIdBare
  , unTreeId

  -- * PartId
  , PartId
--   , unPartId
--   , cmpPartId

  -- * JSON
  -- , encodeAnnoLevelJSONKey
  -- , annoLevelKeyDecoder
  , encodeFileIdJSON
  , fileIdDecoder
  )


import Json.Decode as Decode
import Json.Encode as Encode


---------------------------------------------------
-- File IDs
---------------------------------------------------

-- | File name.
type alias FileName = String


-- | Annotation level.
type alias AnnoLevel = String


-- -- | Annotation level.
-- type AnnoLevel
--   = Orig
--     -- ^ Original file
--   | Syntax
--     -- ^ Syntactic level
--   | Temporal
--     -- ^ Temporal level
--   | Relations
--     -- ^ Temporal relations level


-- -- | Decode AnnoLevel.
-- showAnnoLevel : AnnoLevel -> String
-- showAnnoLevel annoLev =
--     case annoLev of
--         Orig -> "orig"
--         Syntax -> "syntax"
--         Temporal -> "temporal"
--         Relations -> "relations"


-- -- | Decode AnnoLevel.
-- readAnnoLevel : String -> Maybe AnnoLevel
-- readAnnoLevel str =
--     case str of
--         "orig" -> Just Orig
--         "syntax" -> Just Syntax
--         "temporal" -> Just Temporal
--         "relations" -> Just Relations
--         _ -> Nothing


-- | ID of a file.
type alias FileId =
  { fileName  : FileName
    -- ^ The name of the file (typically, the original name)
  , annoLevel : AnnoLevel
    -- ^ The level at which the file is being annotated
  , copyId    : String
    -- ^ ID of the copy of the file (to distinguish several copies of the same
    -- file being annotated at the same level)
  }


-- | Encoded file ID
type alias EncFileId = String


encodeFileId : FileId -> EncFileId
encodeFileId r = String.join ":"
  [ r.fileName
  -- , showAnnoLevel r.annoLevel
  , r.annoLevel
  , r.copyId
  ]


decodeFileId : EncFileId -> Maybe FileId
decodeFileId enc =
    case String.split ":" enc of
        [name, level, cid] -> Just
            { fileName = name
            , annoLevel = level
            , copyId = cid }
        _ -> Nothing


---------------------------------------------------
-- Other
---------------------------------------------------


-- | Annotator identifier.
type alias AnnoName = String


-- | Tree identifier
type TreeId = TreeId TreeIdBare


-- | Tree identifier, bare version.  Useful because `TreeId` has no Ord instance...
type alias TreeIdBare = Int


unTreeId : TreeId -> TreeIdBare
unTreeId (TreeId treeId) = treeId


-- | Internal node identifier
type alias NodeId = Int


type alias Addr = (PartId, NodeId)


---------------------------------------------------
-- Partition IDs
---------------------------------------------------


-- | Partition identifier
type alias PartId = Int


-- cmpPartId : PartId -> PartId -> (PartId, PartId)
-- cmpPartId (PartId tid1) (PartId tid2) =
--     let
--         newRepr = if tid1 < tid2 then tid1 else tid2
--         oldRepr = if tid1 < tid2 then tid2 else tid1
--     in
--         (PartId newRepr, PartId oldRepr)



---------------------------------------------------
-- JSON
---------------------------------------------------


-- encodeAnnoLevelJSON : AnnoLevel -> Encode.Value
-- encodeAnnoLevelJSON = Encode.string << toString


-- encodeAnnoLevelJSONKey : AnnoLevel -> Encode.Value
-- encodeAnnoLevelJSONKey = Encode.string << showAnnoLevel


encodeFileIdJSON : FileId -> Encode.Value
encodeFileIdJSON r =
    Encode.object
        [ ("tag", Encode.string "FileId")
        , ("fileName", Encode.string r.fileName)
        -- , ("annoLevel", encodeAnnoLevelJSON r.annoLevel)
        , ("annoLevel", Encode.string r.annoLevel)
        , ("copyId", Encode.string r.copyId)
        ]


-- annoLevelDecoder : Decode.Decoder AnnoLevel
-- annoLevelDecoder =
--     let
--         readIt x =
--             case x of
--                 "Orig" -> Just Orig
--                 "Syntax" -> Just Syntax
--                 "Temporal" -> Just Temporal
--                 "Relations" -> Just Relations
--                 _ -> Nothing
--     in
--         Decode.map (Maybe.withDefault Orig << readIt) Decode.string


-- annoLevelKeyDecoder : Decode.Decoder AnnoLevel
-- annoLevelKeyDecoder =
--     Decode.map (Maybe.withDefault Orig << readAnnoLevel) Decode.string


fileIdDecoder : Decode.Decoder FileId
fileIdDecoder =
    Decode.map3 FileId
        (Decode.field "fileName" Decode.string)
        -- (Decode.field "annoLevel" annoLevelDecoder)
        (Decode.field "annoLevel" Decode.string)
        (Decode.field "copyId" Decode.string)
