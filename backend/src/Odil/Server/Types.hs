{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


-- | Types of the annotation server.


module Odil.Server.Types
(
-- * Core Types
  FileId
, FileName
, AnnoLevel
, encodeFileId
, decodeFileId

, TreeId
, NodeId
, LeafId
, Addr

, AnnoName
, FileMeta(..)
, AccessLevel (..)
, FileStatus (..)
, defaultMeta

-- * Types
, Sent
, Token (..)
, Tree
, Node (..)
, NodeTyp (..)
, File (..)
, Turn (..)
, Link (..)
, LinkData (..)

-- * Utils
, mkNewFile
, sentFromText
-- , emptyTok
-- , mergeToks
-- , concatToks

-- * JSON
) where


import GHC.Generics

import           Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tree as R
import qualified Data.Text as T
import qualified Data.Aeson as JSON

-- import qualified Odil.Ancor.Types as Ancor
import qualified Odil.AnnoTypes as Anno
import Odil.CoreTypes


-----------
-- Types
-----------


-- | A sentence underlying a syntactic tree.
-- type Sent = T.Text
type Sent = [Token]


-- | Create a sentence from text.
sentFromText :: T.Text -> Sent
sentFromText =
  let mkTok x = Token {orth=x, afterSpace=True}
  in  map mkTok . T.words


-- | Sentence token
data Token = Token
  { orth :: T.Text
    -- ^ Orthographic form (original, as in the source file)
  , afterSpace :: Bool
    -- ^ Is it after space?  NOTE: should be true by default.
  } deriving (Generic, Show, Eq, Ord)


-- | Token concatenation.
emptyTok :: Token
emptyTok = Token {orth = "", afterSpace = False}


-- | Merge two tokens.
mergeToks :: Token -> Token -> Token
mergeToks x y = Token
  { orth = T.concat [orth x, ySpace, orth y]
  , afterSpace = afterSpace x }
  where
    ySpace =
      if afterSpace y
      then " "
      else ""


-- -- | Token concatenation.
-- concatToks :: [Token] -> Token
-- concatToks =
--   let strip tok = tok {orth = T.strip (orth tok)}
--   in  strip . foldl' mergeToks emptyTok


instance Monoid Token where
  mempty = emptyTok
  mappend = mergeToks


-- | A syntactic tree.
type Tree = R.Tree Node


-- | A node of a syntactic tree.
data Node
  = Node
    { nodeId :: NodeId
    , nodeVal :: T.Text
    , nodeTyp :: Maybe NodeTyp
    , nodeComment :: T.Text }
  | Leaf
    { leafId :: NodeId
    , leafVal :: T.Text
      -- ^ NOTE: This can be different than in the corresponding sentence token,
      -- because we preprocess tokens for the sake of parsing!
    , leafPos :: Int
    , leafComment :: T.Text }
  deriving (Generic, Show, Eq, Ord)


-- | Type of the node, together with the corresponding annotation.
data NodeTyp
  = NodeEvent Anno.Event
  | NodeSignal Anno.Signal
  | NodeTimex Anno.Timex
  deriving (Generic, Show, Eq, Ord)


-- | A file.
data File = File
  { treeMap :: M.Map TreeId Tree
    -- ^ The annotated trees
    --
    -- IMPORTANT: the trees are assigned only to partition representatives (see
    -- `partMap`).
    --
    -- UPDATE 23/10/2017: We assume that `TreeId`s determine the order of the
    -- trees in the turn (i.e. the trees with lower id's are first).
    --
    -- UPDATE 23/10/2017: We change the type of the `treeMap` from:
    --   :: M.Map TreeId (Sent, Tree)
    -- to:
    --   :: M.Map TreeId Tree,
    -- where information about sentences is now preserved in `sentMap`.

  , sentMap :: M.Map TreeId Sent
    -- ^ The corresponding (raw) sentences (NEW 23/10/2017)

  , partMap :: M.Map TreeId (S.Set TreeId)
    -- ^ Tree partitions, which groups the trees in sets of trees (NEW
    -- 23/10/2017).
    --
    -- UPDATE 01/11/2017: trees are assigned only to partition representatives.

  , reprMap :: M.Map TreeId TreeId
    -- ^ Each ID points to its direct representative.

  , turns :: [Turn]
    -- ^ The list of turns in the file (we don't preserve the division on
    -- episodes and sections)

  , linkSet :: M.Map Link LinkData

  } deriving (Generic, Show, Eq)


-- | Create a new `File` with singleton partitions from a (treeId -> (sent,
-- tree)) map.
mkNewFile :: M.Map TreeId (Sent, Tree) -> [Turn] -> File
mkNewFile treeMap0 turns =
  File
    { treeMap = fmap snd treeMap0
    , sentMap = fmap fst treeMap0
    , partMap = singPartMap
    , reprMap = singReprMap
    , turns = turns
    , linkSet = M.empty
    }
  where
    singPartMap = M.fromList
      . map (\(i, _) -> (i, S.singleton i))
      . M.toList
      $ treeMap0
    singReprMap = M.fromList
      . map (\(i, _) -> (i, i))
      . M.toList
      $ treeMap0


-- | A turn (related to `Ancor.Turn`) can contain several utterances (and,
-- hence, trees), and for each tree we *might* know who is the author of this
-- utterance (speaker ID, i.e., its position in the `speaker` list). We also
-- *might* know (`speaker`) the list of speakers.
data Turn = Turn
  { speaker :: [T.Text]
  , trees :: M.Map TreeId (Maybe Int)
  } deriving (Generic, Show, Eq)


-- | A link between two nodes in a given file.
data Link = Link
  { from :: Addr
  , to :: Addr }
  deriving (Generic, Show, Eq, Ord)


-- | Additional data assigned to a link.
data LinkData = LinkData
  { signalAddr :: Maybe Addr
  } deriving (Generic, Show, Eq)


-----------
-- Various
-----------


-----------
-- JSON
-----------


instance JSON.FromJSON NodeTyp
instance JSON.ToJSON NodeTyp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Node
instance JSON.ToJSON Node where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Link
instance JSON.ToJSON Link where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.FromJSONKey Link
instance JSON.ToJSONKey Link

instance JSON.FromJSON LinkData
instance JSON.ToJSON LinkData where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Token
instance JSON.ToJSON Token where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Turn
instance JSON.ToJSON Turn where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON File
instance JSON.ToJSON File where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
