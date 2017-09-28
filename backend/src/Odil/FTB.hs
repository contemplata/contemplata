{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Working with the FTB originXML format.


module Odil.FTB
( parseFTB

-- * Conversion
, toPenn
) where


import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Tree as R
import           Data.Maybe (catMaybes)
import           Control.Applicative (optional, some, (*>), (<|>))
import           Control.Monad (guard)

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

import qualified Odil.Penn as P


---------------------------------------------------
-- Local types
---------------------------------------------------


type Tree = R.Tree Node


data Node
  = Node T.Text
  | POS {cat :: T.Text, subcat :: Maybe T.Text}
  | MWE {cat :: T.Text, subcat :: Maybe T.Text}
  | MWEPOS {catint :: T.Text}
  | Orth T.Text
  deriving (Show, Eq, Ord)


---------------------------------------------------
-- Conversion
---------------------------------------------------


-- | Convert a given local tree to the Penn tree.
toPenn :: Tree -> P.Tree
toPenn =
  fmap label
  where
    label (Node x) = x
    label (POS {..}) = cat
    label (MWE {..}) = "MW" `T.append` cat
--       case cat of
--         "ADV" -> "MWADV"
--         "A" -> "MWA"
--         _ -> cat
    label (MWEPOS {..}) = catint
    label (Orth x) = T.concatMap escape x
    escape c = case c of
      '(' -> "-LRB-"
      ')' -> "-RRB-"
      '/' -> "\\/"
      '*' -> "\\*"
      ' ' -> "_"
      _ -> T.singleton c


---------------------------------------------------
-- Parsing combinators
---------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree T.Text) a
type Q a = PolySoup.Q (XmlTree T.Text) a


rootQ :: Q [Tree]
rootQ = named "text" `joinR` every' sentQ


sentQ :: Q Tree
sentQ = fmap (R.Node $ Node "SENT") $
  named "SENT" `joinR` every' nodeQ


nodeQ :: Q Tree
nodeQ = wordQ <|> mweQ <|> internalQ


internalQ :: Q Tree
internalQ = name `join` \tag -> do
  children <- every' nodeQ
  guard . not . null $ children
  return $ R.Node (Node tag) children


wordQ :: Q Tree
wordQ = (named "w" *> catSubCat) `join` \(cat, subcat) ->
  R.Node (POS {cat=cat, subcat=subcat}) . (:[]) <$> first textQ


textQ :: Q Tree
textQ = node . PolySoup.Q $ \tag -> do
  txt <- T.strip <$> getText tag
  guard . not . T.null $ txt
  return $ R.Node (Orth txt) []


mweQ :: Q Tree
mweQ = (named "w" *> catSubCat) `join` \(cat, subcat) -> do
  children <- every' mweWordQ
  guard . not . null $ children
  return $ R.Node (MWE {cat=cat, subcat=subcat}) children


-- | Basically like `wordQ`, but looks at `catint` and not `cat`.
mweWordQ :: Q Tree
mweWordQ = (named "w" *> attr "catint") `join` \catint ->
  R.Node (MWEPOS {catint=catint}) . (:[]) <$> first textQ


catSubCat :: PolySoup.Q (TagSoup.Tag T.Text) (T.Text, Maybe T.Text)
catSubCat = (,) <$> attr "cat" <*> optional (attr "subcat")


---------------------------------------------------
-- Higher-level API
---------------------------------------------------


-- | Parse an entire Ancor file.
parseFTB :: T.Text -> [Tree]
parseFTB =
  parseGen rootQ


-- -- | Parse an entire Trans file.
-- parseTrans :: T.Text -> Episode
-- parseTrans =
--   parseGen transQ


-- | Genering parsing function.
parseGen :: Q [a] -> T.Text -> [a]
parseGen q =
  concat . catMaybes . map (runQ q) . parseForest . TagSoup.parseTags
