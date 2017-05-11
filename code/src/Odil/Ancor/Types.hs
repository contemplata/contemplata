module Odil.Ancor.Types
       (Episode, Section, Turn(..), Elem(..), unElem) where


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


-- | An element of a turn.
type Elem = T.Text


unElem :: Elem -> T.Text
unElem = id


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
