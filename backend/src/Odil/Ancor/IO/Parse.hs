{-# LANGUAGE OverloadedStrings #-}


module Odil.Ancor.IO.Parse (parseAncor, parseTrans, parseToken) where


import Control.Monad (void, guard)
import Control.Applicative (optional, some, (*>), (<|>))
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
         Turn (getSpk spk) . interleave . clearRight . mergeRight <$>
         every' (fmap Left whoQ <|> fmap Right elemQ)
  where
    getSpk spk = case spk of
      Nothing -> []
      Just x -> T.words x
    interleave (Left who : Right elem : xs) = (Just who, elem) : interleave xs
    interleave (Right elem : xs) = (Nothing, elem) : interleave xs
    interleave (_ : xs) = interleave xs
    interleave [] = []


-- | Merge (concatenate) the adjacent `Right` elements.
mergeRight :: [Either a [b]] -> [Either a [b]]
mergeRight (Right y : Right y' : xs) = mergeRight (Right (y ++ y') : xs)
mergeRight (x : xs) = x : mergeRight xs
mergeRight [] = []


-- | Remove the empty `Right` elements.
clearRight :: [Either a [b]] -> [Either a [b]]
clearRight =
  let notEmpty = either (const True) (not . null)
  in  filter notEmpty


whoQ :: Q Who
whoQ =
  let mkWho = Who . read . T.unpack
  in  fmap mkWho . node $ named "Who" *> attr "nb"


elemQ :: Q Elem
elemQ -- note that we cannot use `some` here since it is not a parser, just a Q
   =  regularElemQ
  <|> fmap (:[]) eventTokenQ
  <|> syncQ


regularElemQ :: Q Elem
regularElemQ = node . PolySoup.Q $ \tag -> do
  txt <- T.strip <$> getText tag
  guard . not . T.null $ txt
  return $ parseElem txt


eventTokenQ :: Q Token
eventTokenQ =
  fmap mkElem . node $ named "Event" *> ((,) <$> attr "desc" <*> attr "type")
  where
    mkElem (desc, typ) = case typ of
      "noise" -> Bruit desc
      "pronounce" -> Pronounce desc
      _ -> error "Odil.Ancor.IO.Parse.eventElem: event type unknown"


-- | We parse Sync XML elements because, otherwise, they split our speech turns.
syncQ :: Q Elem
syncQ =
  fmap (const []) . node $ named "Sync"


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
tokenP = pauseP <|> bruitP <|> incoP <|> inaudibleP <|> pronounceP <|> plainP


bruitP :: A.Parser Token
bruitP = do
  A.char '['
  x <- A.string "rire" <|> A.string "bb"
     <|> A.string "tx" <|> A.string "pf"
     <|> A.string "e" -- occurs in ESLO, not sure what it means?
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


pronounceP :: A.Parser Token
pronounceP = do
  A.char '<'
  x <- A.string "pif"
  A.char '>'
  return . Pronounce $ x


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
