{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Contemplata.Config
( Config(..)
) where


import qualified Data.Map.Strict as M

import Dhall
import qualified Data.Aeson as JSON

import qualified Contemplata.Config.Entity as E
import qualified Contemplata.Config.Message as Msg
import qualified Contemplata.Config.Command as Cmd


-- | A closed attribute.
data Config = Config
  { entities :: Vector E.Entity
  , relations :: Vector E.Entity
  , nonTerminals :: Vector Text
  , preTerminals :: Vector Text
  , annoLevels :: Vector Text
  , commands :: Vector (Cmd.Command, Msg.Message)
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
