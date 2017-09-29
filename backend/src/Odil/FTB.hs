{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


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
  | POS {cat :: T.Text, subcat :: Maybe T.Text, mph :: Maybe T.Text}
  | MWE {cat :: T.Text, subcat :: Maybe T.Text, mph :: Maybe T.Text}
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
    label (POS {..}) = posToStanford cat subcat mph
    label (MWE {..}) = "MW" `T.append` cat
    label (MWEPOS {..}) = baseConvert catint
    label (Orth x) = T.concatMap escape x
    escape c = case c of
      '(' -> "-LRB-"
      ')' -> "-RRB-"
      '/' -> "\\/"
      '*' -> "\\*"
      ' ' -> "_"
      _ -> T.singleton c


-- | Convert to a POS tag consistent with the Stanford parser and tokenizer.
posToStanford
  :: T.Text       -- ^ cat
  -> Maybe T.Text -- ^ subcat
  -> Maybe T.Text -- ^ mph
  -> T.Text
posToStanford cat subcat mph = case cat of
  "V" -> T.append cat . may mph $ \x -> case x of
    _ | "K" `T.isPrefixOf` x -> "PP"
      | "Y" `T.isPrefixOf` x -> "IMP"
      | "S" `T.isPrefixOf` x -> "S"
      | "G" `T.isPrefixOf` x -> "PR"
    "W" -> "INF"
    _ -> ""
  "N" -> T.append cat . may subcat $ \x -> case x of
    _ | T.take 1 x == "P" -> "PP"
    _ -> tip x
  "CL" -> cat `T.append` maybe "" tip subcat
  "C" -> cat `T.append` maybe "" tip subcat
  "PRO" -> T.append cat . may subcat $ \case
    "rel" -> "REL"
    "int" -> "WH"
    _ -> ""
  "ADV" -> T.append cat . may subcat $ \case
    "int" -> "WH"
    _ -> ""
  "A" -> may subcat $ \case
    "int" -> "ADJWH"
    _ -> "ADJ"
  "D" -> may subcat $ \case
    "int" -> "DETWH"
    _ -> "DET"
  _ -> baseConvert cat -- `T.append` maybe "" tip subcat
  where
    tip = T.toUpper . T.take 1
    may = flip (maybe "")


-- | The baseline conversion of POS tags.
baseConvert
  :: T.Text -- ^ cat
  -> T.Text
baseConvert x = case x of
  "PONCT" -> "PUNC"
  "D" -> "DET"
  "A" -> "ADJ"
  _ -> x




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
wordQ = (named "w" *> catSubCat) `join` \(cat, subcat, mph) ->
  R.Node (POS {cat=cat, subcat=subcat, mph=mph}) . (:[]) <$> first textQ


textQ :: Q Tree
textQ = node . PolySoup.Q $ \tag -> do
  txt <- T.strip <$> getText tag
  guard . not . T.null $ txt
  return $ R.Node (Orth txt) []


mweQ :: Q Tree
mweQ = (named "w" *> catSubCat) `join` \(cat, subcat, mph) -> do
  children <- every' mweWordQ
  guard . not . null $ children
  return $ R.Node (MWE {cat=cat, subcat=subcat, mph=mph}) children


-- | Basically like `wordQ`, but looks at `catint` and not `cat`.
mweWordQ :: Q Tree
mweWordQ = (named "w" *> attr "catint") `join` \catint ->
  R.Node (MWEPOS {catint=catint}) . (:[]) <$> first textQ


catSubCat :: PolySoup.Q (TagSoup.Tag T.Text) (T.Text, Maybe T.Text, Maybe T.Text)
catSubCat = (,,) <$> attr "cat" <*> optional (attr "subcat") <*> optional (attr "mph")


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
