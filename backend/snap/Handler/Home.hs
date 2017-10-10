{-# LANGUAGE OverloadedStrings #-}


module Handler.Home
( homeHandler
) where


import           Data.Text (Text)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Heist.Interpreted

import           Application


-- | Handler called by the toplevel route '/'.
homeHandler :: AppHandler ()
homeHandler = ifTop $ do
  loginStatus <- with auth isLoggedIn
  heistLocal (bindString "message" $ message loginStatus) $ render "home"
  where
    message :: Bool -> Text
    message False = "You are not logged in."
    message True  = "Now you are logged in."
