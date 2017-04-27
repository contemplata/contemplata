{-# LANGUAGE OverloadedStrings #-}


module Odil.Ancor.IO.Parse (parseAncor) where


import Control.Applicative (optional, (*>), (<|>))
import qualified Data.Text as T
import           Data.Maybe (catMaybes)

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

import Odil.Ancor.Types


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree T.Text) a
type Q a = PolySoup.Q (XmlTree T.Text) a


---------------------------------------------------
-- Parsing combinators
---------------------------------------------------


ancorQ :: Q Episode
ancorQ = named "ANCOR" `joinR` first (named "Trans" `joinR` first episodeQ)


episodeQ :: Q Episode
episodeQ = named "Episode" `joinR` every' sectionQ


sectionQ :: Q Section
sectionQ = named "Section" `joinR` every' turnQ


turnQ :: Q Turn
turnQ =
    (named "Turn" *> optional (attr "speaker")) `join`
    \spk ->
         Turn spk <$> every' elemQ


elemQ :: Q Elem
elemQ = anchorQ <|> regularQ


anchorQ :: Q Elem
anchorQ = Anchor <$> named "anchor" `joinR` first (node text)


regularQ :: Q Elem
regularQ = Regular <$> node text


---------------------------------------------------
-- Higher-level API
---------------------------------------------------


-- parseEpi :: T.Text -> Maybe Episode
-- parseEpi = runQ episodeQ . parseTree . TagSoup.parseTags


-- | Parse an entire Ancor file.
parseAncor :: T.Text -> [Section]
parseAncor =
    concat . catMaybes . map (runQ ancorQ) . parseForest . TagSoup.parseTags
