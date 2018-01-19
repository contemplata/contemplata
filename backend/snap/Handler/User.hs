{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Handler.User
( filesHandler
, postponeHandler
, finishHandler
, passwordHandler
, guideHandler

, ifNotGuest
, ifNotGuestSplice
, currentLoginSplice
) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State
import           Control.Monad (guard, filterM, void)
import qualified Control.Concurrent as C

import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Map.Syntax ((##))
import qualified Data.ByteString as BS

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

import qualified Contemplata.Types as Contemplata
import qualified Contemplata.DB as DB

import qualified Auth as MyAuth
import qualified Config as MyCfg
import           Application


---------------------------------------
-- File*s* handler
---------------------------------------


filesHandler :: AppHandler ()
filesHandler = do
  login <- userName
  -- retrieve the set of files to compare
  compSet <-
    maybe S.empty
      (S.fromList . mapMaybe (Contemplata.decodeFileId . T.decodeUtf8))
    . M.lookup "compare"
    . Snap.rqQueryParams
    <$> Snap.getRequest
  writeList <- filterM (hasAccess Contemplata.Write login)
    . M.toList =<< liftDB DB.fileMap
  readList <- filterM (hasAccess Contemplata.Read login)
    . M.toList =<< liftDB DB.fileMap
  Heist.heistLocal
    ( bindSplices $
      localSplices Contemplata.Write login compSet writeList >>
      localSplices Contemplata.Read login compSet readList >>
      compareSplices compSet
    )
    (Heist.render "user/files")
  where
    hasAccess accLevel login (fileId, fileMeta) =
      (== Just accLevel) <$> liftDB (DB.accessLevel fileId login)
    localSplices accLevel login compSet fileList = do
      let withStatus val = map fst . filter (hasStatus val)
          accLevelStr = T.pack (show accLevel)
      ("newList" `T.append` accLevelStr) ##
        mkFileTable login compSet (withStatus Contemplata.New fileList)
      ("touchedList" `T.append` accLevelStr) ##
        mkTouchedTable accLevel login compSet (withStatus Contemplata.Touched fileList)
      ("doneList" `T.append` accLevelStr) ##
        mkFileTable login compSet (withStatus Contemplata.Done fileList)
    hasStatus val (_, Contemplata.FileMeta{..}) = fileStatus == val

    compareSplices compSet = do
      let body = T.intercalate ", "
            [ Contemplata.encodeFileId fileIdTxt
            | fileIdTxt <- S.toList compSet ]
      "compareBody" ## return
        [ mkLink "Compare" "Compare the selected files" $ T.concat
          ["annotate?", annoParams compSet]
        , X.TextNode ": "
        , X.TextNode body
        ]
      "compareList" ## do
        if S.null compSet
          then return []
          else X.childNodes <$> getParamNode


-- | A list of members.
mkFileTable
  :: Contemplata.AnnoName
  -> S.Set Contemplata.FileId -- ^ The list of files to compare
  -> [Contemplata.FileId]
  -> Splice AppHandler
mkFileTable annoName compSet =
  mapM mkElem
  where

    mkElem fileId = do
      file <- lift . liftDB $ DB.loadFile fileId
--       Just access <- lift . liftDB $ DB.accessLevel fileId annoName
      let version = T.concat
            [ T.pack . show $ Contemplata.annoLevel fileId
            , " (", Contemplata.copyId fileId, ")" ]
      return $ X.Element "tr" []
        [ X.Element "td" []
          [ mkLink (Contemplata.fileName fileId) "Annotate the file" $ T.concat
            ["annotate?", annoParams (S.singleton fileId)]
            -- T.intercalate "/" ["annotate", Contemplata.encodeFileId fileId]
          , X.TextNode " ("
          , mkLink "select" "Select for comparison" $ T.concat
            [ "user/files?"
            , compareParams (S.insert fileId compSet) ]
          , X.TextNode ")"
          ]
        , mkText version
        , mkText (T.pack . show $ Contemplata.numberOfTokens file)
        ]


-- | A list of members.
mkTouchedTable
  :: Contemplata.AccessLevel
  -> Contemplata.AnnoName
  -> S.Set Contemplata.FileId -- ^ The list of files to compare
  -> [Contemplata.FileId]
  -> Splice AppHandler
mkTouchedTable access annoName compSet =
  mapM mkElem
  where
    mkElem fileId = do
      file <- lift . liftDB $ DB.loadFile fileId
      let modifLinks =
            if access < Contemplata.Write
            then []
            else
              [ mkLinkTD "postpone" "Click to postpone the annotation of the file" $
                T.intercalate "/" ["user", "file", Contemplata.encodeFileId fileId, "postpone"]
              , mkLinkTD "finish" "Click to finish the annotation of the file" $
                T.intercalate "/" ["user", "file", Contemplata.encodeFileId fileId, "finish"]
              ]
          version = T.concat
            [ T.pack . show $ Contemplata.annoLevel fileId
            , " (", Contemplata.copyId fileId, ")" ]
      return $ X.Element "tr" [] $
        [ X.Element "td" []
          [ mkLink (Contemplata.fileName fileId) "Annotate the file" $ T.concat
            ["annotate?", annoParams (S.singleton fileId)]
            -- T.intercalate "/" ["annotate", Contemplata.encodeFileId fileId]
          , X.TextNode " ("
          , mkLink "select" "Select for comparison" $ T.concat
            [ "user/files?"
            , compareParams (S.insert fileId compSet) ]
          , X.TextNode ")"
          ]
        , mkText version
        , mkText (T.pack . show $ Contemplata.numberOfTokens file)
        ] ++ modifLinks


---------------------------------------
-- Postpone handler
---------------------------------------


postponeHandler :: AppHandler ()
postponeHandler = do
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ Contemplata.decodeFileId fileIdTxt
  liftDB $ DB.postponeAnnotating fileId
  redirectToTop


finishHandler :: AppHandler ()
finishHandler = do
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ Contemplata.decodeFileId fileIdTxt
  login <- userName
  Just access <- liftDB (DB.accessLevel fileId login)
  guard $ access >= Contemplata.Write
  liftDB $ DB.finishAnnotating fileId
  redirectToTop


redirectToTop :: AppHandler ()
redirectToTop = do
  hrefBase <- do
    cfg <- Snap.getSnapletUserConfig
    liftIO $ MyCfg.fromCfg' cfg "href-base"
  Snap.redirect hrefBase


---------------------------------------
-- Password handler
---------------------------------------


passwordHandler :: AppHandler ()
passwordHandler = ifNotGuest $ do

  --  User's login
  login <- userName
  Just authUser <- MyAuth.authByLogin login

  (passView, passData) <-
    D.runForm "change-password-form"
    ( addChangePasswordForm
    . maybe (Auth.ClearText "") id
    $ Auth.userPassword authUser )

  successHtml <- case passData of
    Nothing -> return []
    Just newPass -> do
      newAuthUser <- liftIO $ Auth.setPassword authUser newPass
      Snap.with auth $ Auth.saveUser newAuthUser
      return
        [ X.Element "div"
          [("class", "alert alert-success")]
          [X.TextNode "Success"]
        ]
  let localSplices = do
        "onSuccess" ## return successHtml

  Heist.heistLocal
    ( bindDigestiveSplices passView
    . bindSplices localSplices )
    (Heist.render "user/password")


-- | Change password form.  Returns the new ClearText password.
addChangePasswordForm
  :: Auth.Password -- ^ The current password
  -> Form T.Text AppHandler BS.ByteString
addChangePasswordForm currPass
  = fmap (\(_, new1, _) -> T.encodeUtf8 new1)
  . D.check "Old password incorrect" checkOld
  . D.check "New passwords differ" checkNew
  $ tripleForm
  where
    tripleForm = (,,)
      <$> "user-oldpass"  .: D.text Nothing
      <*> "user-newpass1" .: D.text Nothing
      <*> "user-newpass2" .: D.text Nothing
    checkOld (oldPass, newPass1, newPass2) =
      Auth.checkPassword (Auth.ClearText $ T.encodeUtf8 oldPass) currPass
    checkNew (oldPass, newPass1, newPass2) =
      newPass1 == newPass2


---------------------------------------
-- Current login
---------------------------------------


-- | Run the contents of the node if the logged user has
-- administrative rights.
currentLoginSplice :: Splice AppHandler
currentLoginSplice = do
  login <- lift userName
  return [X.TextNode login]


---------------------------------------
-- Guest
---------------------------------------


-- | Verify that the admin is logged in.
isGuest :: AppHandler Bool
isGuest = do
  cfg <- Snap.getSnapletUserConfig
  guestLogin <- liftIO $ MyCfg.fromCfg' cfg "guest"
  currentMay <- Snap.with auth Auth.currentUser
  return $ case currentMay of
    Nothing -> False
    Just current -> guestLogin == Auth.userLogin current


-- | Verify that someone is logged in but not the guest.
ifNotGuest :: AppHandler () -> AppHandler ()
ifNotGuest after = do
  cfg <- Snap.getSnapletUserConfig
  guestLogin <- liftIO $ MyCfg.fromCfg' cfg "guest"
  currentLogin <- userName
  if guestLogin /= currentLogin
    then after
    else Snap.writeText "Not authorized"


-- | Run the contents of the node if the logged user has
-- administrative rights.
ifNotGuestSplice :: Splice AppHandler
ifNotGuestSplice = lift isGuest >>= \case
  True -> return []
  False -> X.childNodes <$> getParamNode


---------------------------------------
-- Guide handler
---------------------------------------


guideHandler :: AppHandler ()
guideHandler = do
  Heist.render "user/guide"


---------------------------------------
-- Utils
---------------------------------------


-- | Get the current user name.
userName :: AppHandler Contemplata.AnnoName
userName = do
  Just current <- Snap.with auth Auth.currentUser
  return $ Auth.userLogin current


mkText x = X.Element "td" [] [X.TextNode x]
mkLinkTD x tip href = X.Element "td" [] [mkLink x tip href]
mkLink x tip href = X.Element "a"
  [ ("href", href)
  , ("title", tip) ]
  [X.TextNode x]


compareParams :: S.Set Contemplata.FileId -> T.Text
compareParams compSet = T.intercalate "&"
  [ "compare=" `T.append` Contemplata.encodeFileId fileId
  | fileId <- S.toList compSet ]


annoParams :: S.Set Contemplata.FileId -> T.Text
annoParams fileSet = T.intercalate "&"
  [ "filename=" `T.append` Contemplata.encodeFileId fileId
  | fileId <- S.toList fileSet ]


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
