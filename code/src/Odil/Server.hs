{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


-- | The annotation server.


module Odil.Server
(
-- * Types
  module Odil.Server.Types

-- * Server
, runServer
) where


import Control.Monad (forM_, forever)
import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
import qualified Data.Tree as R

import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS
import qualified Data.Aeson as JSON

import Odil.Server.Types
import qualified Odil.Server.Config as Cfg


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
  T.putStrLn msg
  fileMap <- C.readMVar state
  case msg of
    "files" -> WS.sendTextData conn $
      T.intercalate ", " (M.keys fileMap)
    (getFile fileMap -> Just treeMap) -> WS.sendTextData conn $
      JSON.encode treeMap
    _ -> WS.sendTextData conn $
      msg `T.append` ", meow"


-- getFile :: M.Map FileId File -> T.Text -> Maybe LBS.ByteString
getFile :: M.Map FileId File -> T.Text -> Maybe File
getFile fileMap txt = do
  fileId <- T.stripPrefix "want" txt
  M.lookup fileId fileMap
  -- treeMap <- M.lookup fildId fileMap
  -- return $ JSON.encode treeMap
