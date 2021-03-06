-- {-# LANGUAGE DeriveFunctor #-}


-- | The ANCOR format-related types.


module Contemplata.Ancor.Types
( Episode
, Section
, Turn(..)
, Elem(..)
, Who(..)
, Token(..)
) where


import qualified Data.Text as T


-- | The biggest dialogue unit in Ancor (there is one episode per file).
type Episode = [Section]


-- | A section of a dialogue.
type Section = [Turn]


-- | A turn -- the smallest unit of a dialogue.
data Turn = Turn
    { speaker :: [T.Text]
      -- ^ Possibly several speakers
    , elems :: [(Maybe Who, Elem)]
    } deriving (Show, Eq, Ord)


-- | Who uttered the corresponding element of the turn?
--
-- In the Ancor files (UBS files, at least), this info is marked with a number.
newtype Who = Who {unWho :: Int}
  deriving (Show, Eq, Ord)


-- | An element of a turn. The smallest unit of speech that we are working with.
type Elem = [Token]


-- | Speech chunk.
data Token
  = Plain T.Text
    -- ^ Plain text
  | Bruit T.Text
    -- ^ rire, bb (bruits de bouche), tx (toux), pf (souffle)
  | Incomplete T.Text T.Text
    -- ^ po() or po(mme)
  | Pause T.Text
    -- ^ e or #
  | Inaudible
    -- ^ [pi]
  | Pronounce T.Text
    -- ^ ???
  deriving (Show, Eq, Ord)
