{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A simple DiscoDOP parser client (for French).


module Contemplata.DiscoDOP
(
-- * Types
  Orth
, Pos

-- * Parsing
, parseDOP
, tagParseDOP

-- ** Constraints
, parseConsDOP
, mkConsRequest
) where


import Control.Monad (guard)
import Control.Arrow (second)
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Exception as Exc

import qualified Data.Char as Char
import Text.Printf

import qualified Data.Maybe as May
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Tree as R
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Bytes.VarInt as Byte
import qualified Data.Bytes.Serial as Byte
import qualified Data.Bytes.Put as Byte
import qualified Data.Bytes.Get as Byte

import qualified Network.Wreq as Wreq
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)

import qualified Contemplata.Penn as Penn


----------------------------------------------
-- Base types
----------------------------------------------


-- | A word (orthographic form)
type Orth = T.Text


-- | A part-of-speech
type Pos = T.Text


----------------------------------------------
-- Calling DiscoDOP
----------------------------------------------


-- | The address to make the GET request, based on the sentence to parse.
mkRequest
  :: Bool   -- ^ Perform POS tagging?
  -> Bool   -- ^ Retrieve all parsed trees?
  -> T.Text -- ^ Sentence argument
  -> String
mkRequest postag getAll sentArg =
  base ++ T.unpack sentArg ++ otherArgs
  where
    base = "http://0.0.0.0:5000/parser/parse?sent="
    otherArgs = "&est=rfe&marg=nbest&objfun=mpp&coarse=pcfg" ++
      (if postag then "&postag=True" else "") ++
      (if getAll then "&allderivs=True" else "")


-- | Parse a given, tokenized sentence (in French) with DiscoDOP.
tagParseDOP
  :: [Orth]
  -> IO (Maybe Penn.Tree)
tagParseDOP xs = Exc.handle (onException Nothing) $ do
  r <- Wreq.get $ mkRequest True False (sentArg xs)
  print r
  let parse = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return $ parse >>= Penn.parseTree'
  where
    sentArg = T.intercalate "+"


-- | Parse a given, tokenized and pos-tagged sentence (in French) with DiscoDOP.
parseDOP
  :: [(Orth, Pos)]
  -> IO (Maybe Penn.Tree)
parseDOP xs0 = Exc.handle (onException Nothing) $ do
  let xs = map (second unStanfordPOS) xs0
  r <- Wreq.get $ mkRequest False False (sentArg xs)
  print r
  let parse = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return $ parse >>= Penn.parseTree'
  where
    sentArg =
      let mkArg (orth, pos) = T.concat [orth, "/", pos]
      in  T.intercalate "+" . map mkArg


----------------------------------------------
-- Constraints
----------------------------------------------


-- | The address to make the GET request, based on the sentence to parse.
mkConsRequest
  :: [(T.Text, Int, Int)]
            -- ^ Span constraints
  -> Bool   -- ^ Perform POS tagging?
  -> Bool   -- ^ Retrieve all parsed trees?
  -> T.Text -- ^ Sentence argument
  -> String
mkConsRequest spanConstraints postag getAll sentArg =
  base ++ T.unpack sentArg ++ otherArgs
  where
    base = "http://0.0.0.0:5000/parser/parse?sent="
    otherArgs = "&est=rfe&marg=nbest&objfun=mpp&coarse=pcfg" ++
      (if postag then "&postag=True" else "") ++
      (if getAll then "&allderivs=True" else "") ++
      case spanConstraints of
        [] -> ""
        _ -> "&require=" ++
          (urlEncode . mkList)
          (map mkCons spanConstraints)
    mkCons (label, p, q) = mkList $
      [ quote (T.unpack label)
      , mkList $ map show [p..q] ]
    mkList = bracket "[" "]" . L.intercalate ","
    bracket p q x = p ++ x ++ q
    quote x = "\"" ++ x ++ "\""


-- | Parse a given sentence. All the output trees should satisfy the given
-- constraints (there can be many trees due to syntactic ambiguity).
parseConsDOP
  :: [(T.Text, Int, Int)]
     -- ^ Label/span constraints
  -> [(Orth, Pos)]
     -- ^ Tokens and their POS tags
  -> IO [Penn.Tree]
parseConsDOP cons xs0 = Exc.handle (onException []) $ do
  let xs = map (second unStanfordPOS) xs0
      req = mkConsRequest cons False True (sentArg xs)
  r <- Wreq.get req
  let strParses = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return . maybe [] id $ do
    May.mapMaybe Penn.parseTree' . T.lines <$> strParses
  where
    sentArg =
      let mkArg (orth, pos) = T.concat [orth, "/", pos]
      in  T.intercalate "+" . map mkArg
    simplify (_, i, j) = (i, j)


----------------------------------------------
-- Satisfy
--
-- (not used now, but might be useful later)
----------------------------------------------


-- | Does the tree satisfy all the given constraints?
satisfyAll
  :: [(Int, Int)] -- ^ Span constraints
  -> Penn.Tree
  -> Bool
satisfyAll cons tree = and $ map (flip satisfy tree) cons


-- | Does the tree satisfy the given constraint?
satisfy
  :: (Int, Int) -- ^ The constraint
  -> Penn.Tree
  -> Bool
satisfy (x, y) tree0

  = S.member (x, y)
  . S.fromList
  . map snd
  . R.flatten
  $ tree

  where

    -- A version of the tree with spans
    tree = flip ST.evalState 0 . go $ tree0
    go (R.Node x ts) = case ts of
      [] -> do
        i <- ST.get
        ST.put (i + 1)
        return $ R.Node (x, (i, i)) []
      _ -> do
        ts' <- mapM go ts
        let i = minimum $ map (fst . getSpan) ts'
            j = maximum $ map (snd . getSpan) ts'
        return $ R.Node (x, (i, j)) ts'
    getSpan (R.Node (_, span) _) = span


----------------------------------------------
-- Utils
----------------------------------------------


-- | Convert any exception to the given default value.
onException :: a -> Exc.SomeException -> IO a
onException def _ = return def


-- | Simplify the POS tag from the current Stanford tagset to the standard one.
unStanfordPOS :: Pos -> Pos
unStanfordPOS = id
-- unStanfordPOS xpos = case xpos of
--   "CLS" -> "CL"
--   "CLO" -> "CL"
--   "DET" -> "D"
--   "NC" -> "N"
--   "NPP" -> "N"
--   "ADJ" -> "A"
--   "VINF" -> "V"
--   "VPP" -> "V"
--   _ -> xpos


-- | Return head if exists, or Nothing.
headMay :: [a] -> Maybe a
headMay xs = case xs of
  x : _ -> Just x
  _ -> Nothing


-- | Encode a URL component.
urlEncode :: String -> String
urlEncode = concatMap encode
  where
    encode :: Char -> String
    encode c
      | c == ' ' = "+"
      | c == '"' = "%22"
      | Char.isAlphaNum c || c `elem` ("-._~" :: String) = [c]
      | otherwise = printf "%%%02X" c
