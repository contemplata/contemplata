{-# LANGUAGE LambdaCase #-}


module Handler.Utils
( liftDB
) where


import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as State
import qualified Control.Concurrent as C

import qualified Data.Text as T

import qualified Contemplata.DB as DB
import           Application


-- | Lift the DB-related computation to a handler.
liftDB :: DB.DBT a -> AppHandler a
liftDB dbComp = do
  dbMVar <- State.gets _db
  liftIO . C.withMVar dbMVar $ \odilDB ->
    DB.runDBT odilDB dbComp >>= \case
      Left err -> fail (T.unpack err)
      Right x  -> return x
