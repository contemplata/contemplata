{-# LANGUAGE DeriveFunctor #-}


module Odil.Ancor.Types
       (Episode, Section, Turn(..), Elem(..), Chunk(..)) where


import qualified Data.Text as T


-- | The biggest dialog unit in Ancor (there is one episode per file).
type Episode = [Section]


-- | A section of a dialog.
type Section = [Turn]


-- | A turn -- the smallest unit of a dialog.
data Turn = Turn
    { speaker :: Maybe T.Text
    , elems :: [Elem]
    } deriving (Show, Eq, Ord)


-- | An element of a turn. The smallest unit of speach that we are working with.
-- However, it can be farther divided into chunks, due to the annotations
-- related to various things.
type Elem = [Chunk T.Text]


-- | Speach chunk.
data Chunk a
  = Plain a
    -- ^ Plain text
  | Bruit a
    -- ^ rire, bb (bruits de bouche), tx (toux), pf (souffle)
  | Incomplete a
    -- ^ po() or po(mme)
  | Pause a
    -- ^ e or #
  deriving (Show, Eq, Ord, Functor)


-- -- | An element of a turn.
-- data Elem
--     = Anchor T.Text
--     | Regular T.Text
--     deriving (Show, Eq, Ord)
--
--
-- unElem :: Elem -> T.Text
-- unElem (Anchor x) = x
-- unElem (Regular x) = x
