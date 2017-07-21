{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A simple Stanford parser client (for French). Relies on the standard
-- CoreNLP server (see "edu.stanford.nlp.pipeline.StanfordCoreNLPServer"),
-- tested with version 3.7.0.


module Odil.Stanford
( Orth
, Pos
, parseFR
, parseTokenizedFR
, parsePosFR
-- , docFromPos
-- , parseProto
) where


import Control.Monad (guard)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Maybe (MaybeT(..))

-- import Data.Word (Word8, Word16)
-- import Data.Bits ((.&.), shiftR)
import qualified Data.Char as C
import qualified Data.Text as T
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

import qualified Data.ProtoLens as Proto
import qualified Proto.Odil.Stanford.CoreNLP as CoreNLP


----------------------------------------------
-- Base types
----------------------------------------------


-- | A word (orthographic form)
type Orth = T.Text


-- | A part-of-speech
type Pos = T.Text


----------------------------------------------
-- Calling Stanford
----------------------------------------------


-- | The address to make the POST request.
serverCfg :: String
serverCfg = "http://localhost:9000/?properties={\"annotators\":\"tokenize,ssplit,pos,parse\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\",\"tokenize.language\":\"fr\",\"outputFormat\":\"json\"}"


-- | Parse a given sentence (in French).
parseFR :: T.Text -> IO (Maybe Penn.Tree)
parseFR x = do
  r <- Wreq.post serverCfg (T.encodeUtf8 x)
  let parse = r ^? Wreq.responseBody . key "sentences" . nth 0 . key "parse" . _String
  return $ parse >>= Penn.parseTree'


-- | The alternative server configuration where the tokenization has been already performed.
tokenizedServerCfg :: String
tokenizedServerCfg = "http://localhost:9000/?properties={\"annotators\":\"pos,parse\",\"tokenize.whitespace\":\"true\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\",\"tokenize.language\":\"fr\",\"outputFormat\":\"json\"}"


-- | Parse a given sentence (in French).
--
-- Note that input words are not allowed to contain whitespaces (otherwise, the
-- parser fails and returns `Nothing`).
parseTokenizedFR :: [Orth] -> IO (Maybe Penn.Tree)
parseTokenizedFR xs = do
  guard . all noSpace $ xs
  r <- Wreq.post tokenizedServerCfg . T.encodeUtf8 . T.unwords $ xs
  let parse = r ^? Wreq.responseBody . key "sentences" . nth 0 . key "parse" . _String
  return $ parse >>= Penn.parseTree'
  where
    noSpace = T.all (not . C.isSpace)


----------------------------------------------
-- Parsing with existing POS tags
----------------------------------------------


-- posCfg :: String
-- posCfg = "http://localhost:9000/?properties={\"annotators\":\"parse\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"inputFormat\":\"serialized\",\"outputFormat\":\"serialized\",\"serializer\":\"edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer\",\"enforceRequirements\": \"false\"}"


posCfg :: String
posCfg = "http://localhost:9000/?properties={\"annotators\":\"parse\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"inputFormat\":\"serialized\",\"outputFormat\":\"json\",\"serializer\":\"edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer\",\"enforceRequirements\": \"false\"}"


-- | Parse a given sentence, tokenized and with pre-computed POS tags.
parsePosFR :: [(Orth, Pos)] -> IO (Maybe Penn.Tree)
parsePosFR pos = do
  let docBS = encodeDoc (docFromPos pos)
  r <- Wreq.post posCfg docBS
  let parse = r ^? Wreq.responseBody . key "sentences" . nth 0 . key "parse" . _String
  return $ parse >>= Penn.parseTree'


-- protoCfg :: String
-- protoCfg = "http://localhost:9000/?properties={\"annotators\":\"tokenize,ssplit,pos,parse\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\",\"tokenize.language\":\"fr\",\"outputFormat\":\"serialized\",\"serializer\":\"edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer\"}"
--
--
-- parseProto :: T.Text -> IO ()
-- parseProto x = do
--   r <- Wreq.post protoCfg (T.encodeUtf8 x)
--   let res = r ^? Wreq.responseBody -- . key "sentences" . nth 0 . key "parse" . _String
--   -- return $ parse >>= Penn.parseTree'
--   case res of
--     Nothing -> print "Nothing"
--     Just bs -> print (decodeDoc bs)


-- | Encode the CoreNLP document as a (length-prefixed) bytestring which can be
-- send directly to the Stanford server.
encodeDoc :: CoreNLP.Document -> BL.ByteString
encodeDoc doc =
  let bs = Proto.encodeMessage doc
      bn = Byte.runPutL . Byte.serialize . Byte.VarInt $ BS.length bs
  in  bn `BL.append` BL.fromStrict bs


-- | Decode the CoreNLP document from the (length-prefixed) bytestring.
decodeDoc :: BL.ByteString -> Either String CoreNLP.Document
decodeDoc bs = flip Byte.runGetL bs $ do
  Byte.VarInt (header :: Int) <- Byte.deserialize
  n <- fromIntegral <$> Byte.remaining
  Proto.decodeMessage <$> Byte.getByteString n


-- | Create a Stanford document from a list of words and their POS tags.
docFromPos :: [(Orth, Pos)] -> CoreNLP.Document
docFromPos xs =

  Proto.def
  { CoreNLP._Document'text = text
  , CoreNLP._Document'sentence = [sentence]
  }

  where

    text = T.unwords (map fst xs)

    sentence = Proto.def
      { CoreNLP._Sentence'token = tokens
      , CoreNLP._Sentence'tokenOffsetBegin = 0
      , CoreNLP._Sentence'tokenOffsetEnd = fromIntegral (length xs)
      , CoreNLP._Sentence'sentenceIndex = Just 0
      , CoreNLP._Sentence'characterOffsetBegin = Just 0
      -- NOTE: not sure if info below is 100% correct (encoding?)
      , CoreNLP._Sentence'characterOffsetEnd = Just (fromIntegral $ T.length text)
      , CoreNLP._Sentence'hasRelationAnnotations = Just False
      , CoreNLP._Sentence'hasNumerizedTokensAnnotation = Just False
      , CoreNLP._Sentence'hasCorefMentionsAnnotation = Just False
      }

    tokens = calcOffsets . trimLast . trimFirst $
      [ Proto.def
        { CoreNLP._Token'word = Just orth
        , CoreNLP._Token'pos = Just pos
        , CoreNLP._Token'value = Just orth
        , CoreNLP._Token'before = Just " "
        , CoreNLP._Token'after = Just " "
        , CoreNLP._Token'originalText = Just orth
        , CoreNLP._Token'hasXmlContext = Just False
        }
      | (orth, pos) <- xs ]

    trimFirst =
      let go [] = []
          go (x:xs) = x {CoreNLP._Token'before = Just ""} : xs
      in  go
    trimLast =
      let go [] = []
          go (x:xs) = x {CoreNLP._Token'after = Just ""} : xs
      in  reverse . go . reverse

    calcOffsets =
      let go off [] = []
          go off (x:xs)
            = x
              { CoreNLP._Token'beginChar = Just off
              , CoreNLP._Token'endChar = Just (off + shift)
              }
            : go (off + shift + 1) xs
            where
              shift = maybe 0 (fromIntegral . T.length) (CoreNLP._Token'word x)
      in  go 0
