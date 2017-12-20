-- | Core types

module Edit.Core exposing
  ( FileId
  , AnnoName
  , NodeId
  , Addr
  , Link

  , Focus
  , AnnoLevel(..)

  -- * Encoding
  , encodeFileId
  , decodeFileId

  , TreeId (..)
  , TreeIdBare
  , unTreeId

  -- * PartId
  , PartId
--   , unPartId
--   , cmpPartId

  -- * JSON
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


-- | ID of a file.
type alias FileId =
  { fileName  : FileName
    -- ^ The name of the file (typically, the original name)
  , annoLevel : String
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


-- | Link between two trees.
type alias Link = (Addr, Addr)


-- | Focus selector.
type Focus = Top | Bot


-- | Annotation level
type AnnoLevel
    = Segmentation
    | Syntax
    | Temporal


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


encodeFileIdJSON : FileId -> Encode.Value
encodeFileIdJSON r =
    Encode.object
        [ ("tag", Encode.string "FileId")
        , ("fileName", Encode.string r.fileName)
        , ("annoLevel", Encode.string r.annoLevel)
        , ("copyId", Encode.string r.copyId)
        ]


fileIdDecoder : Decode.Decoder FileId
fileIdDecoder =
    Decode.map3 FileId
        (Decode.field "fileName" Decode.string)
        -- (Decode.field "annoLevel" annoLevelDecoder)
        (Decode.field "annoLevel" Decode.string)
        (Decode.field "copyId" Decode.string)
