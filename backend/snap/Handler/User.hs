{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Handler.User
( filesHandler
) where


import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent as C
import qualified Control.Monad.State.Strict as State

import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Map.Syntax ((##))

import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
import           Heist.Interpreted (bindSplices, Splice)
import qualified Text.XmlHtml as X

import           Odil.Server.Types
import qualified Odil.Server.DB as DB
-- import qualified Odil.Server.Users as Users

import           Application


---------------------------------------
-- File*s* handler
---------------------------------------


filesHandler :: AppHandler ()
filesHandler = do
  fileList <- M.toList <$> liftDB DB.fileMap
  Heist.heistLocal
    (bindSplices $ localSplices fileList)
    (Heist.render "user/files")
  where
    localSplices xs = do
      let withStatus val = fileList . map fst . filter (hasStatus val)
      "newList" ## withStatus New xs
      "touchedList" ## withStatus Touched xs
      "doneList" ## withStatus Done xs
    hasStatus val (_, FileMeta{..}) = fileStatus == val


-- | A list of members.
fileList :: [FileId] -> Splice AppHandler
fileList =
  return . map mkElem
  where
    mkElem fileId = X.Element "li"
      [("class", "list-group-item")]
      [mkLink fileId]
    mkLink fileId = X.TextNode $ encodeFileId fileId
--     mkLink fileId = X.Element "a"
--       [("href", "admin/file/" `T.append` encodeFileId fileId)]
--       [X.TextNode $ encodeFileId fileId]


---------------------------------------
-- User name
---------------------------------------


-- | Verify that the admin is logged in.
userName :: AppHandler AnnoName
userName = do
  Just current <- Snap.with auth Auth.currentUser
  return $ Auth.userLogin current


---------------------------------------
-- DB utils
---------------------------------------


-- | Lift the DB-related computation to a handler.
liftDB :: DB.DBT a -> AppHandler a
liftDB dbComp = do
  dbMVar <- State.gets _db
  liftIO . C.withMVar dbMVar $ \odilDB ->
    DB.runDBT odilDB dbComp >>= \case
      Left err -> fail (T.unpack err)
      Right x  -> return x
