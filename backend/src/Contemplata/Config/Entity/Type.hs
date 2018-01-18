{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Contemplata.Config.Entity.Type
( EntityType(..)
) where


import Dhall
import qualified Data.Aeson as JSON

data EntityType = EntityType
  { among :: Vector Text
    -- ^ The set of potential values of Entity's type
  , def :: Maybe Text
    -- ^ Default type, if any
  } deriving (Generic, Show)

instance Interpret EntityType

instance JSON.FromJSON EntityType
instance JSON.ToJSON EntityType where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
