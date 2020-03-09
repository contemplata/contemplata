{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


-- | The main Contemplata types: `File`, `Token`, `Sent`, `Turn`, etc.
--
-- Re-exports `Contemplata.Types.Core`.


module Contemplata.Types
(
-- * Core Types
  FileId(..)
, FileName
, AnnoLevel(..)
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
-- ** Sentence
, Token (..)
, Sent
, sentFromText
-- ** Tree
, Tree
, Node (..)
-- ** Link
, Link (..)
-- ** File
, File (..)
, Turn (..)
, mkNewFile
, numberOfTokens
) where


import GHC.Generics

import           Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tree as R
import qualified Data.Text as T
import qualified Data.Aeson as JSON

-- import qualified Contemplata.AnnoTypes as Anno
import qualified Contemplata.Anno.Entity as E
import Contemplata.Types.Core


----------------------
-- Sentence
----------------------


-- | Sentence token
data Token = Token
  { orth :: T.Text
    -- ^ Orthographic form (original, as in the source file)
  , afterSpace :: Bool
    -- ^ Is it after space?
  } deriving (Generic, Show, Eq, Ord)


-- | An empty token.
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


instance Semigroup Token where
  (<>) = mergeToks

instance Monoid Token where
  mempty = emptyTok


----------------------
-- Sentence
----------------------


-- | A sentence underlying a syntactic tree. Note that the syntactic tree is not
-- required to use all the tokens in the underlying sentence (see also
-- `leafPos`).
type Sent = [Token]


-- | Create a sentence from text, using whitespaces as token delimiters.
sentFromText :: T.Text -> Sent
sentFromText =
  let mkTok x = Token {orth=x, afterSpace=True}
  in  map mkTok . T.words


----------------------
-- Tree
----------------------


-- | A syntactic tree.
type Tree = R.Tree Node


-- | A node of a syntactic tree.
data Node
  = Node
    { nodeId :: NodeId
      -- ^ Each node in the tree must have a unique ID
    , nodeVal :: T.Text
      -- ^ A textual value assigned to the node
    , nodeTyp :: Maybe E.Entity
      -- ^ Node's type or, alternatively, the corresponding annotation (if any)
    , nodeComment :: T.Text
      -- ^ Annotator's comment, completely optional
    }
  | Leaf
    { leafId :: NodeId
      -- ^ Leaf's equivalent of `nodeId`. The sets of `leafId`s and `nodeId`s
      -- should be disjoint.
    , leafVal :: T.Text
      -- ^ A value assigned to the leaf. NOTE: It can be different than in the
      -- corresponding sentence token, because we preprocess tokens for the sake
      -- of parsing.
    , leafPos :: Int
      -- ^ Position of the leaf in the underlying `Sent`ence. This is needed
      -- because (a) not every token is required to be referenced from the tree,
      -- (b) this allows to represent non-projective trees.
    , leafComment :: T.Text
      -- ^ An equivalent of `nodeComment`
    }
  deriving (Generic, Show, Eq, Ord)


----------------------
-- Link
----------------------


-- | A link between two nodes in a given file.
data Link = Link
  { from :: Addr
    -- ^ Starting point
  , to :: Addr
    -- ^ Ending point
  } deriving (Generic, Show, Eq, Ord)


----------------------
-- File
----------------------


-- | A file.
data File = File
  { turns :: [Turn]
    -- ^ The list of turns in the file. In ANCOR files, there is also a division
    -- on episodes and sections. We don't preserve it here, though, assuming
    -- that it can be easily retrieved.

  , sentMap :: M.Map TreeId Sent
    -- ^ The sentences corresponding to the individual `TreeId`s in the
    -- individual `Turn`s.

  , partMap :: M.Map TreeId (S.Set TreeId)
    -- ^ Tree partitions group sentences into sets of sentences. The trees are
    -- then only assigned to the entire partitions.

  , reprMap :: M.Map TreeId TreeId
    -- ^ Each ID points to its direct representative. This map thus allows to
    -- obtain the Id of the partition to which a given sentence/tree ID belongs,
    -- by subsequently following the pointers.
    --
    -- TODO: this should be probably removed from the model, at least on the
    -- back-end side, as it leads to information redundancy (all we need is
    -- already in the `partMap`).

  , treeMap :: M.Map TreeId Tree
    -- ^ A set of `Tree`s in the file, together with their unique `TreeId`s,
    -- assigned to the individual partitions (see `partMap`).
    --
    -- It is imprortant to keep in mind that:
    -- * The trees are assigned only to partition representatives (see
    --   `partMap`).
    -- * `TreeId`s determine the order of the trees in the turn (i.e. the trees
    --   with lower id's are first).

  , linkMap :: M.Map Link E.Entity
    -- ^ The set of links, together with the corresponding annotations
  } deriving (Generic, Show, Eq)


-- | A turn (related to `Contemplata.Ancor.Types.Turn`) can contain several
-- utterances (and, hence, trees), and for each tree we *might* know who is the
-- author of this utterance (speaker ID, i.e., its position in the `speaker`
-- list). We also *might* know (`speaker`) the list of speakers.
data Turn = Turn
  { speaker :: [T.Text]
  , trees :: M.Map TreeId (Maybe Int)
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
    , linkMap = M.empty
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


-----------
-- Various
-----------


-- | Retrieve the number of tokens in the given file.
numberOfTokens :: File -> Int
numberOfTokens File{..} = sum
  [ length sent
  | (_, sent) <- M.toList sentMap
  ]


-----------
-- JSON
-----------


instance JSON.FromJSON Node
instance JSON.ToJSON Node where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Link
instance JSON.ToJSON Link where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.FromJSONKey Link
instance JSON.ToJSONKey Link

instance JSON.FromJSON Token
instance JSON.ToJSON Token where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON Turn
instance JSON.ToJSON Turn where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON File
instance JSON.ToJSON File where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
