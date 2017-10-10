{-# LANGUAGE OverloadedStrings #-}


-- | A library with some helper functions.


module Util.Form
( showForm
, notEmpty
-- , validEmail
, nameEmptyMsg
, usernameEmptyMsg
, firstnameEmptyMsg
, lastnameEmptyMsg
, passwordEmptyMsg
, emailInvalidMsg
, emailInUseMsg
, usernameInUseMsg
, invalidLoginMsg
, wgEmptyMsg
) where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8

import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.View

import           Application


-- | Bind the elements from the digestive form to the corresponding
-- view template.
showForm :: String -> View Text -> AppHandler ()
showForm prefix form =
    heistLocal (bindDigestiveSplices form) $ render template
  where
    template = BS8.pack $ "forms/" ++ prefix ++ "-form"


-- | Check if a text has the length null.
notEmpty :: Text -> Bool
notEmpty = not . T.null


--------------------------
-- Some error messages...
--------------------------

-- TODO: Perhaps error messages should be specified in a
-- configuration file?

nameEmptyMsg :: Text
nameEmptyMsg  = "please enter a name"

usernameEmptyMsg :: Text
usernameEmptyMsg  = "please enter a username"

firstnameEmptyMsg :: Text
firstnameEmptyMsg = "please enter a first name"

lastnameEmptyMsg :: Text
lastnameEmptyMsg  = "please enter a last name"

passwordEmptyMsg :: Text
passwordEmptyMsg  = "please enter a password"

emailInvalidMsg :: Text
emailInvalidMsg   = "please enter a valid email"

emailInUseMsg :: Text
emailInUseMsg   = "email address already in use"

usernameInUseMsg :: Text
usernameInUseMsg  = "username already in use"

invalidLoginMsg :: Text
invalidLoginMsg   = "wrong username/password combination"

wgEmptyMsg :: Text
wgEmptyMsg   = "please select at least one working group"
