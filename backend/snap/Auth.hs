module Auth
( validLogin
, authByLogin
) where


import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as State

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Maybe (isNothing)

import           Snap
import           Snap.Snaplet.Auth

import           Application


-- | Validates the username password combination.
validLogin :: Text -> ByteString -> AppHandler Bool
validLogin login passwd = do
  -- authMgr <- with auth State.get
  -- authUser <- liftIO $ lookupByLogin authMgr login
  authUser <- authByLogin login
  return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText passwd


-- | Get auth data by login.
authByLogin :: Text -> AppHandler (Maybe AuthUser)
authByLogin login = do
  authMgr <- with auth State.get
  liftIO $ lookupByLogin authMgr login
