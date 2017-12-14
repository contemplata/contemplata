{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Odil.Config.Entity.Type
( EntityType(..)
) where


import Dhall
import qualified Data.Aeson as JSON

data EntityType = EntityType
  { among :: Vector Text
  , def :: Maybe Text
  } deriving (Generic, Show)

instance Interpret EntityType

instance JSON.FromJSON EntityType
instance JSON.ToJSON EntityType where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
