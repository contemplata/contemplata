{-# LANGUAGE OverloadedStrings #-}


module Handler.Anno
( bodySplice
, annoHandler
) where


import           Control.Monad.Trans.Class (lift)

import           Data.Map.Syntax ((##))
import qualified Data.Text as T

import qualified Snap.Snaplet.Heist as Heist
import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import           Heist.Interpreted (bindSplices, Splice)
import qualified Text.XmlHtml as X

import           Application


annoHandler :: AppHandler ()
annoHandler = do
  Heist.heistLocal (bindSplices localSplices) (Heist.render "annotation")
  where
    localSplices = do
      "annoBody" ## bodySplice


bodySplice :: Splice AppHandler
bodySplice = do
  mbUser <- lift $ Snap.with auth Auth.currentUser
  case mbUser of
    Nothing -> return [X.TextNode "access not authorized"]
    Just user ->
      return [html]
      where
        html = X.Element "body" [] [script]
        script = X.Element "script" [("type", "text/javascript")] [text]
        text = X.TextNode $ T.concat
          [ "Elm.Main.fullscreen({userName: \""
          , Auth.userLogin user
          , "\"})"
          ]
