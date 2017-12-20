{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Odil.Config
( Config(..)
) where


import qualified Data.Map.Strict as M

import Dhall
import qualified Data.Aeson as JSON

import qualified Odil.Config.Entity as E
import qualified Odil.Config.Message as Msg
import qualified Odil.Config.Command as Cmd


-- | A closed attribute.
data Config = Config
  { entities :: Vector E.Entity
  , nonTerminals :: Vector Text
  , preTerminals :: Vector Text
  , annoLevels :: Vector Text
  , commands :: Vector (Cmd.Command, Msg.Message)
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
