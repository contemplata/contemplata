{-# LANGUAGE OverloadedStrings #-}


module Odil.Ancor.IO.Parse (parseAncor, parseTrans, parseToken) where


import Control.Monad (void, guard)
import Control.Applicative (optional, (*>), (<|>))
import qualified Data.Text as T
import           Data.Maybe (catMaybes)

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Combinator (lookAhead)

import Odil.Ancor.Types


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree T.Text) a
type Q a = PolySoup.Q (XmlTree T.Text) a


---------------------------------------------------
-- Parsing combinators
---------------------------------------------------


ancorQ :: Q Episode
ancorQ = named "ANCOR" `joinR` first transQ


transQ :: Q Episode
transQ = named "Trans" `joinR` first episodeQ


episodeQ :: Q Episode
episodeQ = named "Episode" `joinR` every' sectionQ


sectionQ :: Q Section
sectionQ = named "Section" `joinR` every' turnQ


turnQ :: Q Turn
turnQ =
  (named "Turn" *> optional (attr "speaker")) `join`
  \spk ->
       -- Turn spk . cleanUp <$> every' elemQ
       Turn spk <$> every' elemQ
  where
    -- cleanUp = filter (not . T.null) . map T.strip


elemQ :: Q Elem
elemQ = node . PolySoup.Q $ \tag -> do
  txt <- T.strip <$> getText tag
  guard . not . T.null $ txt
  return $ parseElem txt


-- elemQ :: Q Elem
-- elemQ = anchorQ <|> regularQ
--
--
-- anchorQ :: Q Elem
-- anchorQ = Anchor <$> named "anchor" `joinR` first (node text)
--
--
-- regularQ :: Q Elem
-- regularQ = Regular <$> node text


---------------------------------------------------
-- Parsing elements
---------------------------------------------------


parseElem :: T.Text -> Elem
parseElem = map parseToken . T.words


parseToken :: T.Text -> Token
parseToken x =
  case A.parseOnly (tokenP <* A.endOfInput) x of
  -- case A.parseOnly tokenP x of
    Left err -> error err
    Right el -> el


tokenP :: A.Parser Token
tokenP = pauseP <|> bruitP <|> incoP <|> inaudibleP <|> plainP


bruitP :: A.Parser Token
bruitP = do
  A.char '['
  x <- A.string "rire" <|> A.string "bb" <|> A.string "tx" <|> A.string "pf"
  A.char ']'
  return . Bruit $ x


inaudibleP :: A.Parser Token
inaudibleP = do
  A.char '['
  A.string "pi"
  A.char ']'
  return Inaudible


incoP :: A.Parser Token
incoP = do
  x <- A.takeTill (== '(') <* A.char '('
  y <- A.takeTill (== ')') <* A.char ')'
  return $ Incomplete x y


pauseP :: A.Parser Token
pauseP = do
  x <- A.char 'e' <|> A.char '#'
  A.endOfInput
  return . Pause . T.singleton $ x


plainP :: A.Parser Token
plainP = do
  x <- A.takeText
  guard . not . T.null $ x
  return (Plain x)


---------------------------------------------------
-- Higher-level API
---------------------------------------------------


-- parseEpi :: T.Text -> Maybe Episode
-- parseEpi = runQ episodeQ . parseTree . TagSoup.parseTags


-- | Parse an entire Ancor file.
parseAncor :: T.Text -> Episode
parseAncor =
  parseGen ancorQ


-- | Parse an entire Trans file.
parseTrans :: T.Text -> Episode
parseTrans =
  parseGen transQ


-- | Genering parsing function.
parseGen :: Q [a] -> T.Text -> [a]
parseGen q =
  concat . catMaybes . map (runQ q) . parseForest . TagSoup.parseTags
