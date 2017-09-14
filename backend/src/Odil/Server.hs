{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}


-- | The annotation server.


module Odil.Server
(
-- * Types
  module Odil.Server.Types

-- * Messages
, ParserTyp (..)
, Request (..)
, Answer (..)

-- * Server
, runServer
) where


import GHC.Generics

import Control.Monad (forM_, forever)
import qualified Control.Exception as Exc
import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as R

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Network.WebSockets as WS

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))

import Odil.Server.Types
import qualified Odil.Server.Config as Cfg
import qualified Odil.Server.DB as DB
import qualified Odil.Stanford as Stanford
import qualified Odil.DiscoDOP as DiscoDOP
import qualified Odil.Penn as Penn


-----------
-- Messages
-----------


-- | The type of parser to use.
data ParserTyp
  = Stanford
  | DiscoDOP
  deriving (Generic, Show)

instance JSON.FromJSON ParserTyp
instance JSON.ToJSON ParserTyp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | Request coming from the client.
data Request
  = GetFiles
  | GetFile FileId
  | SaveFile FileId File
  | ParseSent FileId TreeId ParserTyp [Stanford.Orth]
    -- ^ FileId and TreeId are sent there and back so that it
    -- can be checked that the user did not move elsewhere before
    -- he/she got the answer for this request
  | ParseSentPos FileId TreeId ParserTyp [(Stanford.Orth, Stanford.Pos)]
    -- ^ Similar to `ParseSent`, but with POS information
  deriving (Generic, Show)

instance JSON.FromJSON Request
instance JSON.ToJSON Request where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Answers for the client.
data Answer
  = Files [FileId]
    -- ^ The list of files
  | NewFile FileId File
    -- ^ New file to edit
  | ParseResult FileId TreeId Tree
    -- ^ New file to edit
  | Notification T.Text
    -- ^ New file to edit
  deriving (Show)

instance JSON.ToJSON Answer where
  toJSON x = case x of
    Files xs -> JSON.object
      [ "files" .= xs ]
    NewFile fileId file -> JSON.object
      [ "fileId" .= fileId
      , "file" .= file ]
    ParseResult fileId treeId tree -> JSON.object
      [ "fileId" .= fileId
      , "treeId" .= treeId
      , "tree" .= tree ]
    Notification msg -> JSON.object
      [ "notification" .= msg ]


-----------
-- Model
-----------


-- | Load the DB from a given directory.
loadDB :: FilePath -> IO DB.DB
loadDB dbPath = do
  let db = DB.defaultConf dbPath
  res <- DB.runDBT db DB.createDB
--     mapM_
--       (uncurry DB.saveFile)
--       (M.toList Cfg.tempModel)
  case res of
    Left err -> T.putStrLn $ "Could not create DB: " `T.append` err
    Right _  -> return ()
  return db
  -- Cfg.tempModel


-----------
-- Main
-----------


runServer
  :: FilePath -- ^ DB path
  -> String -- ^ Server address
  -> Int -- ^ Port
  -> IO ()
runServer dbPath serverAddr serverPort = do
  state <- C.newMVar =<< loadDB dbPath
  -- WS.runServer Cfg.serverAddr Cfg.serverPort $ application state
  WS.runServer serverAddr serverPort $ application state


-----------
-- App
-----------


-- | The server application.
application :: C.MVar DB.DB -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- msg <- WS.receiveData conn
  -- clients <- C.readMVar state
  talk conn state


talk :: WS.Connection -> C.MVar DB.DB -> IO ()
talk conn state = forever $ do
  msg <- WS.receiveData conn
  LBC.putStrLn msg

  db <- C.takeMVar state
  flip Exc.finally (C.putMVar state db) $ do

    DB.runDBT db DB.fileNum >>= \case
      Left err -> T.putStrLn err
      Right k  -> putStrLn $
        "Talking; the size of the fileMap is " ++ show k

    case JSON.eitherDecode msg of

      Left err -> do
        let msg = T.concat ["JSON decoding error: ", T.pack err]
        T.putStrLn msg
        WS.sendTextData conn . JSON.encode $ Notification msg

      Right GetFiles -> do
        DB.runDBT db DB.fileSet >>= \case
          Left err -> do
            let msg = T.concat ["GetFiles error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
          Right fs -> do
            let ret = Files (S.toList fs)
            WS.sendTextData conn (JSON.encode ret)

      Right (GetFile fileId) -> do
        DB.runDBT db (DB.loadFile fileId) >>= \case
          Left err -> do
            let msg = T.concat ["GetFile error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
          Right file -> do
            let ret = NewFile fileId file
            WS.sendTextData conn (JSON.encode ret)

      Right (SaveFile fileId file) -> do
        putStrLn "Saving file..."
        DB.runDBT db (DB.saveFile fileId file) >>= \case
          Left err -> do
            let msg = T.concat ["Could not save file: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
          Right () -> do
            putStrLn "Saved"
            let msg = T.concat ["File ", fileId, " saved"]
            WS.sendTextData conn . JSON.encode $ Notification msg

      Right (ParseSent fileId treeId parTyp ws) -> do
        putStrLn "Parsing tokenized sentence..."
        treeMay <- case parTyp of
          Stanford -> Stanford.parseTokenizedFR ws
          DiscoDOP -> DiscoDOP.tagParseDOP Nothing ws
        case treeMay of
          Nothing -> do
            let msg = T.concat ["Could not parse: ", T.unwords ws]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg

      Right (ParseSentPos fileId treeId parTyp ws) -> do
        putStrLn "Parsing tokenized+POSed sentence..."
        treeMay <- case parTyp of
          Stanford -> Stanford.parsePosFR ws
          DiscoDOP -> DiscoDOP.parseDOP Nothing ws
        case treeMay of
          Nothing -> do
            let ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
                msg = T.concat ["Could not parse: ", T.unwords ws']
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode $ Notification msg
