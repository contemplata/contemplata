{-# LANGUAGE DeriveGeneric #-}


module Odil.CoreTypes
( FileId
, TreeId
, NodeId
, LeafId
, Addr

, AnnoName
, FileMeta(..)
, AccessLevel (..)
, defaultMeta
) where


import GHC.Generics

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as JSON


-----------
-- Core Types
-----------


-- | ID of a file.
type FileId = T.Text


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
-- Note: `Read` is the lowest possible access level (an assumption we rely on).
data AccessLevel
  = Read  -- ^ Can only read
  | Write -- ^ Can both write and read
  deriving (Show, Eq, Ord, Generic)


-- | Metadata of a given document.
data FileMeta = FileMeta
  { annoMap :: M.Map AnnoName AccessLevel
    -- ^ Annotators who can modify the file
  } deriving (Show, Eq, Ord, Generic)


defaultMeta :: FileMeta
defaultMeta = FileMeta
  { annoMap = M.empty }


----------------
-- JSON instances
----------------


instance JSON.FromJSON AccessLevel
instance JSON.ToJSON AccessLevel where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON FileMeta
instance JSON.ToJSON FileMeta where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
