{-# LANGUAGE OverloadedStrings #-}


-- | A module which allows to retrieve the list of users (logins) from the
-- Snap Auth JSON-based backend.


module Odil.Server.Users
( listUsers
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Lens ((^@..))

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Lens as Lens

import Odil.Server.Types


-- | List the users present in the given JSON file.
listUsers
  :: FilePath -- ^ The path to the JSON password file
  -> IO [AnnoName]
listUsers passPath = do
  json <- T.readFile passPath
  return . map fst $ json ^@.. Lens.key "loginCache" . Lens.members
