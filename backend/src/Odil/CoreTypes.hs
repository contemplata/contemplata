module Odil.CoreTypes
( FileId
, TreeId
, NodeId
, LeafId
, Addr
) where


import qualified Data.Text as T


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
