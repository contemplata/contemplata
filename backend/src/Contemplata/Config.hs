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


-- | Annotation configuration.
data Config = Config
  { entities :: Vector E.Entity
    -- ^ The list of entity configurations
  , relations :: Vector E.Entity
    -- ^ The list of relation configurations
    --
    -- TODO: The `E.Entity` type in represents here the configuration of
    -- attributes and their values that can be assigned to a given annotation,
    -- whether it's an actual entity assigned to a node, or rather a relation
    -- which links two node.
  , nonTerminals :: Vector Text
    -- ^ The list of non-terminal labels
  , preTerminals :: Vector Text
    -- ^ The list of pre-terminal labels (POS tags)
  , annoLevels :: Vector Text
    -- ^ The list of annotation levels
  , commands :: Vector (Cmd.Command, Msg.Message)
    -- ^ The list of configured commands
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
