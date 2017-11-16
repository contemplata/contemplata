{-# LANGUAGE OverloadedStrings #-}


module Handler.Anno
( bodySplice
, annoHandler
) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Map.Syntax ((##))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Configurator as Cfg

import qualified Snap.Snaplet.Heist as Heist
import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import           Heist.Interpreted (bindSplices, Splice)
import qualified Text.XmlHtml as X

import qualified Odil.Server.Types as Odil
import qualified Odil.Server.DB as DB
-- import qualified Config as Cfg
import           Application
import           Handler.Utils (liftDB)


annoHandler :: AppHandler ()
annoHandler = do
  Heist.heistLocal (bindSplices localSplices) (Heist.render "annotation")
  where
    localSplices = do
      "annoBody" ## bodySplice


bodySplice :: Splice AppHandler
bodySplice = do
  Just fileIdTxt <- fmap T.decodeUtf8 <$> Snap.getParam "filename"
  Just fileId <- return $ Odil.decodeFileId fileIdTxt
  liftIO $ putStrLn "DEBUG: " >> print fileId
  mbUser <- lift $ Snap.with auth Auth.currentUser
  case mbUser of
    Nothing -> return [X.TextNode "access not authorized"]
    Just user -> do
      cfg <- lift Snap.getSnapletUserConfig
      -- Just serverPath <- liftIO $ Cfg.fromCfg cfg "websocket-server"
      -- Just serverPathAlt <- liftIO $ Cfg.fromCfg cfg "websocket-server-alt"
      Just serverPath <- liftIO $ Cfg.lookup cfg "websocket-server"
      Just serverPathAlt <- liftIO $ Cfg.lookup cfg "websocket-server-alt"
      -- Mark the file as being annotated
      lift . liftDB $ DB.startAnnotating fileId
      let html = X.Element "body" [] [script]
          script = X.Element "script" [("type", "text/javascript")] [text]
          mkArg key val = T.concat [key, ": \"", val, "\""]
          mkArgs = T.intercalate ", " . map (uncurry mkArg)
          text = X.TextNode $ T.concat
            [ "Elm.Main.fullscreen({"
            , mkArgs
              [ ("userName", Auth.userLogin user)
              , ("fileId", Odil.encodeFileId fileId)
              , ("websocketServer", serverPath)
              , ("websocketServerAlt", serverPathAlt)
              ]
            , "})"
            ]
--             [ "Elm.Main.fullscreen({userName: \""
--             , Auth.userLogin user
--             , "\"})"
--             ]
      return [html]
