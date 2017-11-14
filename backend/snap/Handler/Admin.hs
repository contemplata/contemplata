{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Handler.Admin
( createUserHandler
, passwordHandler
, filesHandler
, fileHandler
, fileAddAnnoHandler
, fileRemoveAnnoHandler
, fileChangeAccessAnnoHandler

-- * Utis
, isAdmin
, ifAdmin
, ifAdminSplice
) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (guard, (<=<))

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Configurator as Cfg
import           Data.Map.Syntax ((##))


import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
import           Heist.Interpreted (bindSplices, Splice)
import           Heist (getParamNode)
import qualified Text.XmlHtml as X

import           Text.Digestive.Form (Form, (.:))
import qualified Text.Digestive.Form as D
import           Text.Digestive.Heist (bindDigestiveSplices)
import qualified Text.Digestive.View as D
import qualified Text.Digestive.Snap as D


import           Odil.Server.Types
import qualified Odil.Server.DB as DB
import qualified Odil.Server.Users as Users

import qualified Auth as MyAuth
import qualified Config as MyCfg
import           Application
import           Handler.Utils (liftDB)


---------------------------------------
-- Handlers
---------------------------------------


-- | Create user.
createUserHandler :: AppHandler ()
createUserHandler = ifAdmin $ do
  -- isAdmin
  Just login <- Snap.getParam "login"
  Just passw <- Snap.getParam "passw"
  res <- Snap.with auth $ Auth.createUser (T.decodeUtf8 login) passw
  case res of
    Left err -> Snap.writeText . T.pack $ show res
    Right _  -> Snap.writeText "Success"


-- | Change password.
passwordHandler :: AppHandler ()
passwordHandler = ifAdmin $ do
  -- isAdmin
  Just login <- Snap.getParam "login"
  Just passw <- Snap.getParam "passw"
  Just authUser <- MyAuth.authByLogin (T.decodeUtf8 login)
  authUser' <- liftIO $ Auth.setPassword authUser passw
  res <- Snap.with auth $ Auth.saveUser authUser'
  case res of
    Left err -> Snap.writeText . T.pack $ show res
    Right _  -> Snap.writeText "Success"


---------------------------------------
-- File*s* handler
---------------------------------------


filesHandler :: AppHandler ()
filesHandler = ifAdmin $ do
  fileSet <- liftDB DB.fileSet
  Heist.heistLocal (bindSplices $ localSplices fileSet) (Heist.render "admin/files")
  where
    localSplices fileSet = do
      "fileList" ## fileList (S.toList fileSet)


-- | A list of members.
fileList :: [FileId] -> Splice AppHandler
fileList =
  return . map mkElem
  where
    mkElem fileId = X.Element "li"
      [("class", "list-group-item")]
      [mkLink fileId]
    mkLink fileId = X.Element "a"
      [("href", "admin/file/" `T.append` encodeFileId fileId)]
      [X.TextNode $ encodeFileId fileId]


---------------------------------------
-- File handler
---------------------------------------


fileHandler :: AppHandler ()
fileHandler = ifAdmin $ do

  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ decodeFileId fileIdTxt

  allAnnotators <- do
    cfg <- Snap.getSnapletUserConfig
    passPath <- liftIO $ MyCfg.fromCfg' cfg "password" -- "pass.json"
    liftIO $ Users.listUsers passPath
  (annoView, annoName) <- D.runForm
    "add-anno-form"
    (addAnnoForm allAnnotators)

  case annoName of
    Nothing -> return ()
    -- Just an -> modifyAppl fileName an
    Just an -> liftDB $ DB.addAnnotator fileId an Read

  annotations <- liftDB $ M.toList . annoMap <$> DB.loadMeta fileId
  let localSplices = do
        "fileName" ## return
          [X.TextNode fileIdTxt]
        "currentAnnotators" ## return
          (map (mkElem fileIdTxt) annotations)

  Heist.heistLocal
    ( bindSplices localSplices .
      bindDigestiveSplices annoView )
    ( Heist.render "admin/file" )

  where

    mkElem fileName (annoName, access) = X.Element "tr" []
        [ mkText annoName
        , mkLink "remove" "Click to remove" $
          T.intercalate "/" ["admin", "file", fileName, "remanno", annoName]
        , mkLink (T.pack $ show access) "Click to change" $
          T.intercalate "/" ["admin", "file", fileName, "changeaccess", annoName]
        ]

    mkText x = X.Element "td" [] [X.TextNode x]
    mkLink x tip href = X.Element "td" [] [X.Element "a"
        [ ("href", href)
        , ("title", tip) ]
        [X.TextNode x] ]


-- | Login form for a user.
addAnnoForm :: [T.Text] -> Form T.Text AppHandler T.Text
addAnnoForm anns =
  let double x = (x, x)
  in  "anno-name" .: D.choice (map double anns) Nothing


---------------------------------------
-- File modification handlers
---------------------------------------


-- | Add annotator to a file handler.
fileAddAnnoHandler :: AppHandler ()
fileAddAnnoHandler = ifAdmin $ do
  Just fileNameBS <- Snap.getParam "filename"
  Just fileName <- (return . decodeFileId) (T.decodeUtf8 fileNameBS)
  Just annoName <- fmap T.decodeUtf8 <$> Snap.getParam "annoname"
  liftDB $ DB.addAnnotator fileName annoName Read
  Snap.redirect $ "/admin/file/" `BS.append` fileNameBS


-- | Remmove annotator from a file handler.
fileRemoveAnnoHandler :: AppHandler ()
fileRemoveAnnoHandler = ifAdmin $ do
  Just fileNameBS <- Snap.getParam "filename"
  Just fileName <- (return . decodeFileId) (T.decodeUtf8 fileNameBS)
  Just annoName <- fmap T.decodeUtf8 <$> Snap.getParam "annoname"
  liftDB $ DB.remAnnotator fileName annoName
  Snap.redirect $ "/admin/file/" `BS.append` fileNameBS


-- | Remmove annotator from a file handler.
fileChangeAccessAnnoHandler :: AppHandler ()
fileChangeAccessAnnoHandler = ifAdmin $ do
  Just fileNameBS <- Snap.getParam "filename"
  Just fileName <- (return . decodeFileId) (T.decodeUtf8 fileNameBS)
  Just annoName <- fmap T.decodeUtf8 <$> Snap.getParam "annoname"
  liftDB $ DB.changeAccessAnnotator fileName annoName
  Snap.redirect $ "/admin/file/" `BS.append` fileNameBS


---------------------------------------
-- DB utils
---------------------------------------


---------------------------------------
-- Utils
---------------------------------------


-- | Verify that the admin is logged in.
isAdmin :: AppHandler Bool
isAdmin = do
  cfg <- Snap.getSnapletUserConfig
  adminLogin <- liftIO $ MyCfg.fromCfg' cfg "admin"
  --
  -- NOTE: this handler must not fail because it is used in
  -- the splice `ifAdminSplce`! Hence we need to explicitely
  -- handle the `Nothing` value, and not with pattern matching.
  --
  -- Just current <- Snap.with auth Auth.currentUser
  -- return $ adminLogin == Auth.userLogin current
  --
  currentMay <- Snap.with auth Auth.currentUser
  return $ case currentMay of
    Nothing -> False
    Just current -> adminLogin == Auth.userLogin current


-- | Verify that the admin is logged in.
ifAdmin :: AppHandler () -> AppHandler ()
ifAdmin after = do
  cfg <- Snap.getSnapletUserConfig
  adminLogin <- liftIO $ MyCfg.fromCfg' cfg "admin"
  Just current <- Snap.with auth Auth.currentUser
  if adminLogin == Auth.userLogin current
    then after
    else Snap.writeText "Not authorized"


-- | Run the contents of the node if the logged user has
-- administrative rights (currently, every logged-in user
-- has administrative rights).
ifAdminSplice :: Splice AppHandler
ifAdminSplice = lift isAdmin >>= \case
  False -> return []
  True -> X.childNodes <$> getParamNode
