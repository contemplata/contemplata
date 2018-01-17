{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Handler.Anno
( annoHandler
, adjuHandler
) where


import           Control.Monad (when, guard, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Configurator as Cfg
import           Data.Map.Syntax ((##))

import qualified Snap.Snaplet.Heist as Heist
import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import           Heist.Interpreted (bindSplices, Splice)
import qualified Text.XmlHtml as X

import qualified Contemplata.Types as Contemplata
import qualified Contemplata.Server.DB as DB
-- import qualified Config as Cfg
import           Application
import           Handler.Utils (liftDB)


---------------------------------------
-- Regular annotation
---------------------------------------


annoHandler :: AppHandler ()
annoHandler = do
  Heist.heistLocal (bindSplices localSplices) (Heist.render "annotation")
  where
    localSplices = do
      "annoBody" ## annoBodySplice


annoBodySplice :: Splice AppHandler
annoBodySplice = do

--   Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
--   Just fileId <- return $ Contemplata.decodeFileId fileIdTxt

  fileSet <-
    maybe S.empty
      (S.fromList . mapMaybe (Contemplata.decodeFileId . T.decodeUtf8))
    . M.lookup "filename"
    . Snap.rqQueryParams
    <$> Snap.getRequest

  -- Ensure that the set of annotated files is not empty
  guard . not $ S.null fileSet

  -- liftIO $ putStrLn "DEBUG: " >> print fileId
  mbUser <- lift $ Snap.with auth Auth.currentUser
  case mbUser of
    Nothing -> return [X.TextNode "access not authorized"]
    Just user -> do
      let login = Auth.userLogin user
      cfg <- lift Snap.getSnapletUserConfig
      Just serverPath <- liftIO $ Cfg.lookup cfg "websocket-server"
      Just serverPathAlt <- liftIO $ Cfg.lookup cfg "websocket-server-alt"
      -- Mark the files as being annotated
      lift . liftDB $ do
        forM_ (S.toList fileSet) $ \fileId -> do
          DB.accessLevel fileId login >>= \case
            Just acc -> when
              (acc >= Contemplata.Write)
              (DB.startAnnotating fileId)
            _ -> return ()
      let html = X.Element "body" [] [script]
          script = X.Element "script" [("type", "text/javascript")] [text]
          mkVal val = T.concat ["\"", val, "\""]
          mkArg key val = T.concat [key, ": ", mkVal val]
          -- mkArgs = T.intercalate ", " . map (uncurry mkArg)
          between p q xs = T.concat [p, xs, q]
          text = X.TextNode $ T.concat
            [ "Elm.Main.fullscreen({"
            , T.intercalate ", "
              [ mkArg "userName" login
              -- , ("fileId", Contemplata.encodeFileId . head $ S.toList fileSet)
              , T.concat
                [ "fileIds: "
                , between "[" "]" $ T.intercalate ","
                  (map (mkVal . Contemplata.encodeFileId) (S.toList fileSet))
                ]
              , mkArg "websocketServer" serverPath
              , mkArg "websocketServerAlt" serverPathAlt
              ]
            , "})"
            ]
--             [ "Elm.Main.fullscreen({userName: \""
--             , Auth.userLogin user
--             , "\"})"
--             ]
      return [html]


---------------------------------------
-- Adjudication
---------------------------------------


adjuHandler :: AppHandler ()
adjuHandler = do
  Heist.heistLocal (bindSplices localSplices) (Heist.render "annotation")
  where
    localSplices = do
      "annoBody" ## adjuBodySplice


adjuBodySplice :: Splice AppHandler
adjuBodySplice = do
  -- The main file
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "main"
  Just mainId <- return $ Contemplata.decodeFileId fileIdTxt
  -- The other file, for comparison
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "comp"
  Just compId <- return $ Contemplata.decodeFileId fileIdTxt
  mbUser <- lift $ Snap.with auth Auth.currentUser
  case mbUser of
    Nothing -> return [X.TextNode "access not authorized"]
    Just user -> do
      let login = Auth.userLogin user
      cfg <- lift Snap.getSnapletUserConfig
      Just serverPath <- liftIO $ Cfg.lookup cfg "websocket-server"
      Just serverPathAlt <- liftIO $ Cfg.lookup cfg "websocket-server-alt"
--       -- Mark the file as being annotated
--       lift . liftDB $ do
--         DB.accessLevel fileId login >>= \case
--           Just acc -> when
--             (acc >= Contemplata.Write)
--             (DB.startAnnotating fileId)
--           _ -> return ()
      let html = X.Element "body" [] [script]
          script = X.Element "script" [("type", "text/javascript")] [text]
          mkArg key val = T.concat [key, ": \"", val, "\""]
          mkArgs = T.intercalate ", " . map (uncurry mkArg)
          text = X.TextNode $ T.concat
            [ "Elm.Main.fullscreen({"
            , mkArgs
              [ ("userName", login)
              , ("fileId", Contemplata.encodeFileId mainId)
              , ("compId", Contemplata.encodeFileId compId)
              , ("websocketServer", serverPath)
              , ("websocketServerAlt", serverPathAlt)
              ]
            , "})"
            ]
      return [html]
