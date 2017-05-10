{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}


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
import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
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
  deriving (Show)

instance JSON.ToJSON Answer where
  toJSON x = case x of
    Files xs -> JSON.object
      [ "files" .= xs ]
    NewFile fileId file -> JSON.object
      [ "fileId" .= fileId
      , "file" .= file ]


-----------
-- Model
-----------


type ServerState = M.Map FileId File


newServerState :: ServerState
newServerState = Cfg.tempModel


-----------
-- Main
-----------


runServer :: IO ()
runServer = do
  state <- C.newMVar newServerState
  WS.runServer Cfg.serverAddr Cfg.serverPort $ application state


-----------
-- App
-----------


-- | The server application.
application :: C.MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- msg <- WS.receiveData conn
  -- clients <- C.readMVar state
  talk conn state


talk :: WS.Connection -> C.MVar ServerState -> IO ()
talk conn state = forever $ do
  msg <- WS.receiveData conn
  LBC.putStrLn msg
  fileMap <- C.readMVar state
  putStrLn $ "Talking; the size of the fileMap is " ++ show (M.size fileMap)
  case JSON.eitherDecode msg of

    Left err -> do
      putStrLn "JSON decoding error:"
      putStrLn err

    Right GetFiles -> do
      let ret = Files (M.keys fileMap)
      WS.sendTextData conn (JSON.encode ret)


    Right (GetFile fileId) -> case M.lookup fileId fileMap of
      Nothing -> return ()
      Just treeMap -> do
        let ret = NewFile fileId treeMap
        WS.sendTextData conn (JSON.encode ret)

    Right (SaveFile fileId file) -> do
      putStrLn "Saving file..."
      C.modifyMVar_ state (return . M.insert fileId file)
      putStrLn "Saved"
      -- fileMapNow <- C.readMVar state
      -- putStrLn $ "The size is now: " ++ show (M.size fileMapNow)


-- -- getFile :: M.Map FileId File -> T.Text -> Maybe LBS.ByteString
-- getFile :: M.Map FileId File -> T.Text -> Maybe File
-- getFile fileMap txt = do
--   fileId <- T.stripPrefix "want " txt
--   M.lookup fileId fileMap
--   -- treeMap <- M.lookup fildId fileMap
--   -- return $ JSON.encode treeMap
