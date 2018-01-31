{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Contemplata.Types.Core
(
-- * File-related
  FileName
, AnnoLevel(..)
, FileId(..)
, encodeFileId
, decodeFileId

-- * Tree-related
, TreeId
, NodeId
, LeafId
, Addr

-- * Metadata
, AnnoName
, AccessLevel (..)
, FileStatus (..)
, FileMeta(..)
, defaultMeta
) where


import GHC.Generics
import Control.Applicative ((<|>), (<$))
import Text.Read (readMaybe, Lexeme(Ident))

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON


----------------------
-- File-related Types
----------------------


-- | File name.
type FileName = T.Text


-- | Annotation level (e.g. orig(inal), syntax, termporal, etc.).
type AnnoLevel = T.Text


-- | ID of a file.
data FileId = FileId
  { fileName  :: FileName
    -- ^ The base name of the file
  , annoLevel :: AnnoLevel
    -- ^ The level at which the file is being annotated
  , copyId    :: T.Text
    -- ^ ID of the copy of the file (to distinguish several copies of the same
    -- file being annotated at the same level)
  } deriving (Show, Eq, Ord, Generic)


encodeFileId :: FileId -> T.Text
encodeFileId FileId{..} = T.intercalate ":"
  [ fileName
  , annoLevel
  , copyId
  ]


decodeFileId :: T.Text -> Maybe FileId
decodeFileId enc = do
  [name, level, cid] <- Just $ T.split (==':') enc
  return FileId
    { fileName = name
    , annoLevel = level
    , copyId = cid }


--------------------
-- Other Core Types
--------------------


-- | ID of a syntactic tree.
type TreeId = Int


-- | Node ID.
type NodeId = Int


-- | Leaf ID; corresponds to the order of words in a given sentence.
type LeafId = Int


-- | A node address.
type Addr = (TreeId, NodeId)


----------------
-- File metadata
----------------


-- | Annotator name.
type AnnoName = T.Text


-- | Level of acces to a file.
--
-- NOTE: `Read` is the lowest possible access level (an assumption we rely on).
data AccessLevel
  = Read  -- ^ Can only read
  | Write -- ^ Can both write and read
  deriving (Show, Eq, Ord, Generic)


-- | File status
data FileStatus
  = New        -- ^ Waiting to be annotated
  | Touched    -- ^ Annotation in progress
  | Done       -- ^ Annotation finished
  deriving (Show, Eq, Ord, Generic)


-- | Metadata of a given document.
data FileMeta = FileMeta
  { annoMap :: M.Map AnnoName AccessLevel
    -- ^ Annotators who can modify the file
  , fileStatus :: FileStatus
    -- ^ The current file status
  } deriving (Show, Eq, Ord, Generic)


defaultMeta :: FileMeta
defaultMeta = FileMeta
  { annoMap = M.empty
  , fileStatus = New
  }


----------------
-- JSON instances
----------------


instance JSON.FromJSON AccessLevel
instance JSON.ToJSON AccessLevel where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON FileStatus
instance JSON.ToJSON FileStatus where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON FileMeta
instance JSON.ToJSON FileMeta where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON FileId
instance JSON.FromJSONKey FileId where
  fromJSONKey = JSON.FromJSONKeyText $ \enc -> do
    let err = "Types.Core.decodeFileId: cannot decode \""
              ++ T.unpack enc ++ "\""
    maybe (error err) id $ decodeFileId enc
instance JSON.ToJSON FileId where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.ToJSONKey FileId where
  toJSONKey = JSON.toJSONKeyText encodeFileId
