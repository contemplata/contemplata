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


-----------
-- Messages
-----------


-- | Request coming from the client.
data Request
  = GetFiles
  | GetFile FileId
  | SaveFile FileId File
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
    Notification msg -> JSON.object
      [ "notification" .= msg ]


-----------
-- Model
-----------


-- | Load the DB from a given directory.
loadDB :: FilePath -> IO DB.DB
loadDB dbPath = do
  let db = DB.defaultConf dbPath
  res <- DB.runDBT db $ do
    DB.createDB
    mapM_
      (uncurry DB.saveFile)
      (M.toList Cfg.tempModel)
  case res of
    Left err -> T.putStrLn $ "Could not load DB: " `T.append` err
    Right _  -> return ()
  return db
  -- Cfg.tempModel


-----------
-- Main
-----------


runServer :: FilePath -> IO ()
runServer dbPath = do
  state <- C.newMVar =<< loadDB dbPath
  WS.runServer Cfg.serverAddr Cfg.serverPort $ application state


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
        putStrLn "JSON decoding error:"
        putStrLn err

      Right GetFiles -> do
        DB.runDBT db DB.fileSet >>= \case
          Left err -> T.putStrLn err
          Right fs -> do
            let ret = Files (S.toList fs)
            WS.sendTextData conn (JSON.encode ret)

      Right (GetFile fileId) -> do
        DB.runDBT db (DB.loadFile fileId) >>= \case
          Left err -> T.putStrLn err
          Right file -> do
            let ret = NewFile fileId file
            WS.sendTextData conn (JSON.encode ret)

  --     Right (GetFile fileId) -> case M.lookup fileId fileMap of
  --       Nothing -> return ()
  --       Just treeMap -> do
  --         let ret = NewFile fileId treeMap
  --         WS.sendTextData conn (JSON.encode ret)

      Right (SaveFile fileId file) -> do
        putStrLn "Saving file..."
        DB.runDBT db (DB.saveFile fileId file) >>= \case
          Left err -> do
            T.putStrLn err
            let msg = T.concat ["Could not save file; ", err]
            WS.sendTextData conn . JSON.encode $ Notification msg
          Right () -> do
            putStrLn "Saved"
            let msg = T.concat ["File ", fileId, " saved"]
            WS.sendTextData conn . JSON.encode $ Notification msg


  --     Right (SaveFile fileId file) -> do
  --       putStrLn "Saving file..."
  --       C.modifyMVar_ state (return . M.insert fileId file)
  --       putStrLn "Saved"
  --       -- fileMapNow <- C.readMVar state
  --       -- putStrLn $ "The size is now: " ++ show (M.size fileMapNow)


-- -- getFile :: M.Map FileId File -> T.Text -> Maybe LBS.ByteString
-- getFile :: M.Map FileId File -> T.Text -> Maybe File
-- getFile fileMap txt = do
--   fileId <- T.stripPrefix "want " txt
--   M.lookup fileId fileMap
--   -- treeMap <- M.lookup fildId fileMap
--   -- return $ JSON.encode treeMap
