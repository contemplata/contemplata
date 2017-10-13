{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (guard)
import           Control.Lens (makeLenses)
import           Control.Applicative ((<|>))
import qualified Control.Concurrent as C
-- import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.State.Strict as State
import qualified Control.Exception as Exc

-- import           Options.Applicative

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import           Data.Monoid ((<>))

import qualified Odil.Server.DB as DB
import qualified Odil.Server as Server
import qualified Odil.Server.Config as ServerCfg

import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
-- import           Heist.Interpreted (Splice, bindSplices, Splices)
import           Snap.Snaplet.Auth.Backends.JsonFile (initJsonFileAuthManager)
import qualified Snap.Snaplet.Session as Session
import qualified Snap.Snaplet.Session.Backends.CookieSession as Session
import qualified Snap.Snaplet.Heist as Heist
-- import qualified Snap.Core as Core
-- import           Snap.Core (Snap)
import qualified Snap.Util.FileServe as FileServe

-- import qualified Snap.Http.Server as Server
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as SnapWS

-- import qualified Snap.Snaplet as Snaplet
import qualified Handler.Login as Login
import qualified Handler.Anno as Anno
import qualified Handler.Admin as Admin
import qualified Config as Cfg
import           Application


---------------------------------------
-- App
---------------------------------------


routes :: [(BS.ByteString, AppHandler ())]
routes =
  [ ("/ws", wsHandler)
  -- , ("/style.css", FileServe.serveFileAs "text/css" "html/style.css")
  , ("/annotation", Anno.annoHandler)
  , ("/public", publicHandler)
  , ("/login", Login.loginHandler)
  , ("/logout", Login.logoutHandler)
  -- , ("/public", publicHandler)
  , ("/admin/createuser", Admin.createUserHandler)
  , ("/admin/password", Admin.passwordHandler)
  , ("/admin/files", Admin.filesHandler)
  , ("/admin/file/:filename", Admin.fileHandler)
  , ("/admin/file/:filename/addanno/:annoname", Admin.fileAddAnnoHandler)
  , ("/admin/file/:filename/remanno/:annoname", Admin.fileRemoveAnnoHandler)
  , ("", Snap.ifTop rootHandler)
  ]


---------------------------------------
-- Main handlers
---------------------------------------


rootHandler :: AppHandler ()
rootHandler =
  loginHandler <|> Snap.redirect "annotation"
  where
    loginHandler = do
      guard . not =<< Snap.with auth Auth.isLoggedIn
      Snap.redirect "login"


publicHandler :: AppHandler ()
publicHandler = FileServe.serveDirectory "resources/public"


echoHandler :: AppHandler ()
echoHandler = do
  param <- Snap.getParam "echoparam"
  maybe
    (Snap.writeBS "must specify echo/param in URL")
    Snap.writeBS param


wsHandler :: AppHandler ()
wsHandler = do
  guard =<< Snap.with auth Auth.isLoggedIn
  dbMVar <- State.gets _db
  SnapWS.runWebSocketsSnap $
    \pending -> Server.application dbMVar pending
                `Exc.catch` catchHandschakeExc
                `Exc.catch` catchConnectionExc
                `Exc.catch` catchSomeExc
  where
    catchHandschakeExc (e :: WS.HandshakeException) = do
      putStrLn "WS: catched the following websocket HandshakeException:"
      putStrLn $ show e
    catchConnectionExc (e :: WS.ConnectionException) = do
      putStrLn "WS: catched the following websocket ConnectionException:"
      putStrLn $ show e
    catchSomeExc (e :: Exc.SomeException) = do
      putStrLn "WS: catched the following SomeException:"
      putStrLn $ show e


----------------------------------
-- Initialization
----------------------------------


-- -- | Application splices.
-- splices :: Splices (Splice AppHandler)
-- splices = do
--   "annoBody" ## Anno.bodySplice


-- | Application initialization.
appInit :: Snap.SnapletInit App App
appInit = Snap.makeSnaplet "snap-odil" "ODIL" Nothing $ do
  cfg <- Snap.getSnapletUserConfig
  dbPath <- liftIO $ Cfg.fromCfgDef cfg "DB" "DB"
  db <- liftIO $ C.newMVar =<< Server.loadDB dbPath
  s <- Snap.nestSnaplet "sess" sess $
       Session.initCookieSessionManager "site_key.txt" "_cookie" Nothing Nothing
  passPath <- liftIO $ Cfg.fromCfgDef cfg "password" "pass.json"
  a <- Snap.nestSnaplet "auth" auth $
       initJsonFileAuthManager Auth.defAuthSettings sess passPath
  tempPath <- liftIO $ Cfg.fromCfgDef cfg "templates" "templates"
  h <- Snap.nestSnaplet "heist" heist $ Heist.heistInit tempPath
  Auth.addAuthSplices h auth
--   Heist.modifyHeistState
--     $ bindSplices splices
--     -- $ bindAttributeSplices attrSplices
--     -- . bindSplices splices
  Snap.addRoutes routes
  return $ App db h s a


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = do
  Snap.serveSnaplet Snap.defaultConfig appInit
