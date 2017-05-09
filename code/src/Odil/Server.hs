{-# LANGUAGE OverloadedStrings #-}


-- | The annotation server.


module Odil.Server (runServer) where


import Control.Monad (forM_, forever)
import qualified Control.Concurrent as C

import qualified Data.Text as T
import qualified Network.WebSockets as WS


-----------
-- Model
-----------


-- | The only thing the server has to know for know is the connection (if any).
-- For the moment only one connection is allowed.
type ServerState = ()


newServerState :: ServerState
newServerState = ()


-----------
-- Main
-----------


runServer :: IO ()
runServer = do
    state <- C.newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state


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
  WS.sendTextData conn $ msg `T.append` ", meow"
--   C.readMVar state >>= broadcast
--     (user `mappend` ": " `mappend` msg)
