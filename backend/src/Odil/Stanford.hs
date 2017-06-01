{-# LANGUAGE OverloadedStrings #-}


-- | A simple Stanford parser client (for French). Relies on the standard
-- CoreNLP server (see "edu.stanford.nlp.pipeline.StanfordCoreNLPServer"),
-- tested with version 3.7.0.


module Odil.Stanford
( parseFR
) where


-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- import qualified Network.URI as URI
import qualified Network.Wreq as Wreq
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)

import qualified Odil.Penn as Penn


-- | The address to make the POST request.
serverAddr :: String
serverAddr = "http://localhost:9000/?properties={\"annotators\":\"tokenize,ssplit,pos,parse\",\"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\",\"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\",\"tokenize.language\":\"fr\",\"outputFormat\":\"json\"}"


-- | Parse a given sentence (in French).
parseFR :: T.Text -> IO (Maybe Penn.Tree)
parseFR x = do
  r <- Wreq.post serverAddr (T.encodeUtf8 x)
  let parse = r ^? Wreq.responseBody . key "sentences" . nth 0 . key "parse" . _String
  return $ parse >>= Penn.parseTree'
