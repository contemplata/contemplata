{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


-- | Types of the annotation server.


module Odil.Server.Types
(
-- * Types
  FileId
, TreeId
, NodeId
, LeafId
, Tree
, Node (..)
, File

-- * JSON
) where


import GHC.Generics
import qualified Data.Map.Strict as M
import qualified Data.Tree as R
import qualified Data.Text as T
import qualified Data.Aeson as JSON


-----------
-- Types
-----------


-- | ID of a file.
type FileId = T.Text


-- | ID of a syntactic tree.
type TreeId = T.Text


-- | Node ID.
type NodeId = Int


-- | Leaf ID; corresponds to the order of words in a given sentence.
type LeafId = Int


-- | A syntactic tree.
type Tree = R.Tree Node


-- | A node of a syntactic tree.
data Node
  = Node
    { nodeId :: NodeId
    , nodeVal :: T.Text }
  | Leaf
    { leafId :: NodeId
    , leafVal :: T.Text
    , leafPos :: Int }
  deriving (Generic, Show, Eq, Ord)


-- | A file.
type File = M.Map TreeId Tree


-----------
-- Various
-----------


-----------
-- JSON
-----------


instance JSON.ToJSON Node where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
