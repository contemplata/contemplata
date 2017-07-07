{-# LANGUAGE OverloadedStrings #-}


-- | A simple Stanford parser client (for French). Relies on the standard
-- CoreNLP server (see "edu.stanford.nlp.pipeline.StanfordCoreNLPServer"),
-- tested with version 3.7.0.


module Odil.Stanford
( parseFR
, parseTokenizedFR
) where


import Control.Monad (guard)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- import qualified Network.URI as URI
import qualified Network.Wreq as Wreq
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)

import qualified Odil.Penn as Penn


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
parseTokenizedFR :: [T.Text] -> IO (Maybe Penn.Tree)
parseTokenizedFR xs = do
  guard . all noSpace $ xs
  r <- Wreq.post tokenizedServerCfg . T.encodeUtf8 . T.unwords $ xs
  let parse = r ^? Wreq.responseBody . key "sentences" . nth 0 . key "parse" . _String
  return $ parse >>= Penn.parseTree'
  where
    noSpace = T.all (not . C.isSpace)
