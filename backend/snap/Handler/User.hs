{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Handler.User
( filesHandler
, postponeHandler
, finishHandler
) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Concurrent as C
import qualified Control.Monad.State.Strict as State

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Map.Syntax ((##))

import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
import           Heist.Interpreted (bindSplices, Splice)
import qualified Text.XmlHtml as X

import qualified Odil.Server.Types as Odil
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
    localSplices fileList = do
      let withStatus val = map fst . filter (hasStatus val)
      "newList" ## mkFileTable (withStatus Odil.New fileList)
      "touchedList" ## mkTouchedTable (withStatus Odil.Touched fileList)
      "doneList" ## mkFileTable (withStatus Odil.Done fileList)
    hasStatus val (_, Odil.FileMeta{..}) = fileStatus == val


-- | A list of members.
mkFileTable :: [Odil.FileId] -> Splice AppHandler
mkFileTable =
  mapM mkElem
  where

    mkElem fileId = do
      file <- lift . liftDB $ DB.loadFile fileId
      return $ X.Element "tr" []
        [ mkLink (Odil.fileName fileId) "annotate" $
          T.intercalate "/" ["annotate", Odil.encodeFileId fileId]
        , mkText (T.pack . show $ Odil.annoLevel fileId)
        , mkText (T.pack . show $ Odil.numberOfTokens file)
--         , mkLink "remove" "Click to remove" $
--           T.intercalate "/" ["admin", "file", fileName, "remanno", annoName]
--         , mkLink (T.pack $ show access) "Click to change" $
--           T.intercalate "/" ["admin", "file", fileName, "changeaccess", annoName]
        ]


-- | A list of members.
mkTouchedTable :: [Odil.FileId] -> Splice AppHandler
mkTouchedTable =
  mapM mkElem
  where

    mkElem fileId = do
      file <- lift . liftDB $ DB.loadFile fileId
      return $ X.Element "tr" []
        [ mkLink (Odil.fileName fileId) "annotate" $
          T.intercalate "/" ["annotate", Odil.encodeFileId fileId]
        , mkText (T.pack . show $ Odil.annoLevel fileId)
        , mkText (T.pack . show $ Odil.numberOfTokens file)
        , mkLink "postpone" "Click to postpone the annotation of the file" $
          T.intercalate "/" ["user", "file", Odil.encodeFileId fileId, "postpone"]
        , mkLink "finish" "Click to finish the annotation of the file" $
          T.intercalate "/" ["user", "file", Odil.encodeFileId fileId, "finish"]
        ]


---------------------------------------
-- Postpone handler
---------------------------------------


postponeHandler :: AppHandler ()
postponeHandler = do
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ Odil.decodeFileId fileIdTxt
  liftDB $ DB.postponeAnnotating fileId
  Snap.redirect "/"


finishHandler :: AppHandler ()
finishHandler = do
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ Odil.decodeFileId fileIdTxt
  liftDB $ DB.finishAnnotating fileId
  Snap.redirect "/"


---------------------------------------
-- Utils
---------------------------------------


-- | Verify that the admin is logged in.
userName :: AppHandler Odil.AnnoName
userName = do
  Just current <- Snap.with auth Auth.currentUser
  return $ Auth.userLogin current


mkText x = X.Element "td" [] [X.TextNode x]
mkLink x tip href = X.Element "td" [] [X.Element "a"
    [ ("href", href)
    , ("title", tip) ]
    [X.TextNode x] ]


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
