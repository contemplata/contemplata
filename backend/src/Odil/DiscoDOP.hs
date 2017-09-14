{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A simple DiscoDOP parser client (for French).


module Odil.DiscoDOP
( Orth
-- , Pos
, parseDOP
, tagParseDOP
-- , parseTokenizedFR
-- , parsePosFR
-- -- , docFromPos
-- -- , parseProto
) where


import Control.Monad (guard)
import Control.Arrow (second)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Maybe (MaybeT(..))

-- import Data.Word (Word8, Word16)
-- import Data.Bits ((.&.), shiftR)
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
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
  -> T.Text -- ^ Sentence argument
  -> String
mkRequest spanConstraint postag sentArg =
  base ++ T.unpack sentArg ++ otherArgs
  where
    base = "http://0.0.0.0:5000/parser/parse?sent="
    otherArgs = "&est=rfe&marg=nbest&objfun=mpp&coarse=pcfg" ++
      if postag then "&postag=True" else "" ++
      case spanConstraint of
        Nothing -> ""
        Just (x, y) -> "&constraint=" ++ show x ++ "," ++ show y


-- | Parse a given, tokenized sentence (in French) with DiscoDOP.
tagParseDOP
  :: Maybe (Int, Int) -- ^ Span constraint
  -> [Orth]
  -> IO (Maybe Penn.Tree)
tagParseDOP spanConstraint xs = do
  r <- Wreq.get $ mkRequest spanConstraint True (sentArg xs)
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
parseDOP spanConstraint xs0 = do
  let xs = map (second unStanfordPOS) xs0
  r <- Wreq.get $ mkRequest spanConstraint False (sentArg xs)
  print r
  let parse = fmap
        (T.strip . T.decodeUtf8 . BL.toStrict)
        (r ^? Wreq.responseBody)
  return $ parse >>= Penn.parseTree'
  where
    sentArg =
      let mkArg (orth, pos) = T.concat [orth, "/", pos]
      in  T.intercalate "+" . map mkArg


-- | Simplify the POS tag from the current Stanford tagset to the standard one.
unStanfordPOS :: Pos -> Pos
unStanfordPOS xpos = case xpos of
  "CLS" -> "CL"
  "CLO" -> "CL"
  "DET" -> "D"
  "NC" -> "N"
  "ADJ" -> "A"
  _ -> xpos
