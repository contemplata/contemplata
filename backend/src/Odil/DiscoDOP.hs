{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A simple DiscoDOP parser client (for French).


module Odil.DiscoDOP
( Orth
-- , Pos
, parseDOP
, tagParseDOP
, parseDOP'
) where


import Control.Monad (guard)
import Control.Arrow (second)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.State.Strict as ST

import qualified Control.Exception as Exc

-- import Data.Word (Word8, Word16)
-- import Data.Bits ((.&.), shiftR)
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

-- import qualified Network.URI as URI
import qualified Network.Wreq as Wreq
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)

import qualified Odil.Penn as Penn

-- import qualified Data.ProtoLens as Proto
-- import qualified Proto.Odil.Stanford.CoreNLP as CoreNLP


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
  :: Maybe (Int, Int)
            -- ^ Span constraint (optional)
  -> Bool   -- ^ Perform POS tagging?
  -> Bool   -- ^ Retrieve all parsed trees?
  -> T.Text -- ^ Sentence argument
  -> String
mkRequest spanConstraint postag getAll sentArg =
  base ++ T.unpack sentArg ++ otherArgs
  where
    base = "http://0.0.0.0:5000/parser/parse?sent="
    otherArgs = "&est=rfe&marg=nbest&objfun=mpp&coarse=pcfg" ++
      (if postag then "&postag=True" else "") ++
      (if getAll then "&allderivs=True" else "") ++
      case spanConstraint of
        Nothing -> ""
        Just (x, y) -> "&constraint=" ++ show x ++ "," ++ show y


-- | Parse a given, tokenized sentence (in French) with DiscoDOP.
tagParseDOP
  :: Maybe (Int, Int) -- ^ Span constraint
  -> [Orth]
  -> IO (Maybe Penn.Tree)
tagParseDOP spanConstraint xs = Exc.handle ignoreException $ do
  r <- Wreq.get $ mkRequest spanConstraint True False (sentArg xs)
  print r
  let parse = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return $ parse >>= Penn.parseTree'
  where
    sentArg = T.intercalate "+"


-- | Parse a given, tokenized and pos-tagged sentence (in French) with DiscoDOP.
parseDOP
  :: Maybe (Int, Int) -- ^ Span constraint
  -> [(Orth, Pos)]
  -> IO (Maybe Penn.Tree)
parseDOP spanConstraint xs0 = Exc.handle ignoreException $ do
  let xs = map (second unStanfordPOS) xs0
  r <- Wreq.get $ mkRequest spanConstraint False False (sentArg xs)
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
-- Calling DiscoDOP, several derivations
----------------------------------------------


-- | Parse a given, tokenized French sentence with DiscoDOP.
-- A version of `tagParseDOP` which enumerates all derivations returned by
-- DiscoDOP and choosing the one which satisfies the given constraint (or the
-- first tree, if none).
parseDOP'
  :: [(Int, Int)] -- ^ Span constraints
  -> [(Orth, Pos)]
  -> IO (Maybe Penn.Tree)
parseDOP' cons xs0 = Exc.handle ignoreException $ do
  let xs = map (second unStanfordPOS) xs0
  r <- Wreq.get $ mkRequest Nothing False True (sentArg xs)
  let strParses = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return $ do
    parses <- May.mapMaybe Penn.parseTree' . T.lines <$> strParses
    headMay $ filter (satisfyAll cons) parses
  where
    sentArg =
      let mkArg (orth, pos) = T.concat [orth, "/", pos]
      in  T.intercalate "+" . map mkArg


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


-- | Convert any exception to `Nothing`.
ignoreException :: Exc.SomeException -> IO (Maybe a)
ignoreException _ = return Nothing


-- | Simplify the POS tag from the current Stanford tagset to the standard one.
unStanfordPOS :: Pos -> Pos
unStanfordPOS xpos = case xpos of
  "CLS" -> "CL"
  "CLO" -> "CL"
  "DET" -> "D"
  "NC" -> "N"
  "ADJ" -> "A"
  _ -> xpos


-- | Return head if exists, or Nothing.
headMay :: [a] -> Maybe a
headMay xs = case xs of
  x : _ -> Just x
  _ -> Nothing
