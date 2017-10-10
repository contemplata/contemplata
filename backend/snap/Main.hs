{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Monad.IO.Class (liftIO)
import           Control.Lens (makeLenses)
import           Control.Applicative ((<|>))
import qualified Control.Concurrent as C
-- import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.State.Strict as State
import qualified Control.Exception as Exc

-- import           Options.Applicative

import qualified Data.ByteString as BS
-- import           Data.Monoid ((<>))

import qualified Odil.Server.DB as DB
import qualified Odil.Server as Server
import qualified Odil.Server.Config as ServerCfg

import qualified Snap as Snap
-- import qualified Snap.Core as Core
-- import           Snap.Core (Snap)
import qualified Snap.Util.FileServe as FileServe
-- import qualified Snap.Http.Server as Server
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as SnapWS

-- import qualified Snap.Snaplet as Snaplet


---------------------------------------
-- Application state
---------------------------------------


-- | Application state.
data App = App
  { _db :: C.MVar DB.DB
    -- ^ Database
  }


makeLenses ''App


-- | Application handler monad.
type AppHandler = Snap.Handler App App


---------------------------------------
-- App
---------------------------------------


routes :: [(BS.ByteString, AppHandler ())]
routes =
--   [ ("foo", Snap.writeBS "bar")
--   , ("echo/:echoparam", echoHandler)
  [ ("ws", wsHandler)
  , ("style.css", FileServe.serveFileAs "text/css" "html/style.css")
  -- , ("style.css", echoHandler)
  , ("", FileServe.serveFile "html/index.html")
  ]
--   Snap.ifTop (Snap.writeBS "hello world") <|>
--   Snap.route
--     [ ("foo", Snap.writeBS "bar")
--     , ("echo/:echoparam", echoHandler)
--     , ("odil", FileServe.serveFile "html/index.html")
--     , ("odil/ws", Snap.redirect "ws://127.0.0.1:9161")
--     ] <|>
--   Snap.dir "static" (FileServe.serveDirectory ".")


echoHandler :: AppHandler ()
echoHandler = do
  param <- Snap.getParam "echoparam"
  maybe
    (Snap.writeBS "must specify echo/param in URL")
    Snap.writeBS param


wsHandler :: AppHandler ()
wsHandler = do
  dbMVar <- State.gets _db
  SnapWS.runWebSocketsSnap $
    \pending -> Server.application dbMVar pending
                `Exc.catch` catchHandschakeExc
                `Exc.catch` catchConnectionExc
  where
    catchHandschakeExc (e :: WS.HandshakeException) = do
      putStrLn "WS: catched the following websocket HandshakeException:"
      putStrLn $ show e
    catchConnectionExc (e :: WS.ConnectionException) = do
      putStrLn "WS: catched the following websocket ConnectionException:"
      putStrLn $ show e


----------------------------------
-- Initialization
----------------------------------


-- | Application initialization.
appInit :: FilePath -> Snap.SnapletInit App App
appInit dbPath = Snap.makeSnaplet "snap-odil" "ODIL" Nothing $ do
  db <- liftIO $ C.newMVar =<< Server.loadDB dbPath
  Snap.addRoutes routes
  let app = App db
  return app


--     s <- nestSnaplet "sess" sess $
--         initCookieSessionManager "site_key.txt" "_cookie" Nothing
--     d <- nestSnaplet "db" db pgsInit
--     a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
--     h <- nestSnaplet "heist" heist $ heistInit "templates"
--     addAuthSplices h auth
--     modifyHeistState
--         $ bindAttributeSplices attrSplices
--         . bindSplices splices
--     addRoutes routes
--     return $ App h s d a


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = do
  let dbPath = "/home/kuba/work/odil/data/DB-new"
  Snap.serveSnaplet Snap.defaultConfig (appInit dbPath)
