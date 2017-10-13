{-# LANGUAGE DeriveGeneric #-}


module Odil.CoreTypes
( FileId
, TreeId
, NodeId
, LeafId
, Addr

, AnnoName
, FileMeta(..)
, defaultMeta
) where


import GHC.Generics

import qualified Data.Set as S
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


-- | Metadata of a given document.
data FileMeta = FileMeta
  { annoSet :: S.Set AnnoName
    -- ^ Annotators who can modify the file
  } deriving (Show, Eq, Ord, Generic)


defaultMeta :: FileMeta
defaultMeta = FileMeta
  { annoSet = S.empty }


instance JSON.FromJSON FileMeta
instance JSON.ToJSON FileMeta where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
