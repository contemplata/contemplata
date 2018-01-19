{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}


module Application
( App (..)
, AppHandler
, heist
, sess
, auth
, db
) where


import qualified Control.Concurrent as C

import qualified Contemplata.DB as DB

import qualified Snap as Snap
import qualified Snap.Snaplet.Auth as Auth
import qualified Snap.Snaplet.Heist as Heist
import qualified Snap.Snaplet.Session as Session

import           Control.Lens (makeLenses)


----------------------------------
-- Application snaplet
----------------------------------


-- | Application state.
data App = App
  { _db       :: C.MVar DB.DB
    -- ^ Database
  , _heist    :: Snap.Snaplet (Heist.Heist App)
    -- ^ Heist templating system snaplet
  , _sess     :: Snap.Snaplet Session.SessionManager
    -- ^ Session snaplet
  , _auth     :: Snap.Snaplet (Auth.AuthManager App)
    -- ^ Authentication snaplet
  }


makeLenses ''App


instance Heist.HasHeist App where
    heistLens = Snap.subSnaplet heist


-- | Application handler monad.
type AppHandler = Snap.Handler App App
