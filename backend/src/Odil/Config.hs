{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Odil.Config
( Config(..)
) where


import Dhall
import qualified Data.Aeson as JSON

import qualified Odil.Config.Entity as E


-- | A closed attribute.
data Config = Config
  { entities :: Vector E.Entity
  , nonTerminals :: Vector Text
  , preTerminals :: Vector Text
  , annoLevels :: Vector Text
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
