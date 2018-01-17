{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
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

import qualified Contemplata.Server.DB as DB
import qualified Contemplata.Server as Server
import qualified Contemplata.Server.Config as ServerCfg

import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
import           Snap.Snaplet.Auth.Backends.JsonFile (initJsonFileAuthManager)
import qualified Snap.Snaplet.Session as Session
import qualified Snap.Snaplet.Session.Backends.CookieSession as Session
import qualified Snap.Snaplet.Heist as Heist
-- import qualified Snap.Core as Core
-- import           Snap.Core (Snap)
import qualified Snap.Util.FileServe as FileServe

import           Heist (Splices)
import           Heist.Interpreted (Splice, bindSplices, bindAttributeSplices)
import           Data.Map.Syntax ((##))
import qualified Text.XmlHtml as X

-- import qualified Snap.Http.Server as Server
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as SnapWS

-- import qualified Snap.Snaplet as Snaplet
import qualified Handler.Login as Login
import qualified Handler.Anno as Anno
import qualified Handler.Admin as Admin
import qualified Handler.User as User
import qualified Config as Cfg
import           Application


---------------------------------------
-- App
---------------------------------------


routes :: [(BS.ByteString, AppHandler ())]
routes =
  -- Generic handlers first
  [ ("/ws", wsHandler)
  -- , ("/style.css", FileServe.serveFileAs "text/css" "html/style.css")
  -- , ("/annotate/:filename", Anno.annoHandler)
  , ("/annotate", Anno.annoHandler)
  -- , ("/adjudicate/:main/:comp", Anno.adjuHandler)
  , ("/public", publicHandler)
  , ("/login", Login.loginHandler)
  , ("/logout", Login.logoutHandler)

  -- User-related handlers
  , ("/user/files", User.filesHandler)
  , ("/user/password", User.passwordHandler)
  , ("/user/guide", User.guideHandler)
  , ("/user/file/:filename/postpone", User.postponeHandler)
  , ("/user/file/:filename/finish", User.finishHandler)

  -- Admin-related handlers
  , ("/admin/createuser", Admin.createUserHandler)
  , ("/admin/password", Admin.passwordHandler)
  , ("/admin/files", Admin.filesHandler)
  , ("/admin/users", Admin.usersHandler)
  , ("/admin/file/:filename", Admin.fileHandler)
  , ("/admin/file/:filename/addanno/:annoname", Admin.fileAddAnnoHandler)
  , ("/admin/file/:filename/remanno/:annoname", Admin.fileRemoveAnnoHandler)
  , ("/admin/file/:filename/changeaccess/:annoname", Admin.fileChangeAccessAnnoHandler)
  , ("/admin/file/:filename/changestatus", Admin.fileChangeStatusHandler)
  , ("/admin/file/:filename/remove", Admin.fileRemoveHandler)
  , ("/admin/json/:filename", Admin.fileDownloadHandler)
  , ("/admin/upload", Admin.fileUploadHandler)
  , ("", Snap.ifTop rootHandler)
  ]


---------------------------------------
-- Main handlers
---------------------------------------


rootHandler :: AppHandler ()
rootHandler =
  loginHandler <|> adminHandler <|> annoHandler
  where
    loginHandler = do
      guard . not =<< Snap.with auth Auth.isLoggedIn
      Snap.redirect "login"
    adminHandler = do
      guard =<< Admin.isAdmin
      Snap.redirect "admin/files"
    -- annoHandler = Snap.redirect "annotation"
    annoHandler = Snap.redirect "user/files"


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
  cfg <- Snap.getSnapletUserConfig
  SnapWS.runWebSocketsSnap $
    \pending -> Server.application dbMVar cfg pending
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


-- | Application splices.
globalSplices :: Splices (Splice AppHandler)
globalSplices = do
  "ifAdmin" ## Admin.ifAdminSplice
  "ifNotAdmin" ## Admin.ifNotAdminSplice
  "ifNotGuest" ## User.ifNotGuestSplice
  "currentLogin" ## User.currentLoginSplice
  "hrefBase" ## hrefBase
  where
    hrefBase = lift $ do
      cfg <- Snap.getSnapletUserConfig
      base <- liftIO $ Cfg.fromCfg' cfg "href-base"
      return [X.Element "base" [("href", base)] []]


-- -- | Application splices.
-- globalAttrSplices :: Splices (AttrSplice AppHandler)
-- globalAttrSplices = do
--   "hrefBase" ## hrefBase
--   where
--     hrefBase = do


-- | Application initialization.
appInit :: Snap.SnapletInit App App
appInit = Snap.makeSnaplet "snap-odil" "ODIL" Nothing $ do
  cfg <- Snap.getSnapletUserConfig
  dbPath <- liftIO $ Cfg.fromCfgDef cfg "DB" "DB"
  db <- liftIO $ C.newMVar =<< Server.loadDB dbPath
  s <- Snap.nestSnaplet "sess" sess $
       Session.initCookieSessionManager "site_key.txt" "_cookie" Nothing Nothing
  passPath <- liftIO $ Cfg.fromCfg' cfg "password" -- "pass.json"
  a <- Snap.nestSnaplet "auth" auth $
       initJsonFileAuthManager Auth.defAuthSettings sess passPath
  tempPath <- liftIO $ Cfg.fromCfgDef cfg "templates" "templates"
  h <- Snap.nestSnaplet "heist" heist $ Heist.heistInit tempPath
  Auth.addAuthSplices h auth
  Heist.modifyHeistState
    -- (bindAttributeSplices attrSplices)
    (bindSplices globalSplices)
  Snap.addRoutes routes
  return $ App db h s a


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = do
  Snap.serveSnaplet Snap.defaultConfig appInit
