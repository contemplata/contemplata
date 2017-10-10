module Auth
( validLogin
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
  authMgr <- with auth State.get
  authUser <- liftIO $ lookupByLogin authMgr login
  return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText passwd
