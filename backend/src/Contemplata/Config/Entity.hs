{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module Contemplata.Config.Entity
( Entity(..)
) where


import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Dhall
import qualified Data.Aeson as JSON

import qualified Contemplata.Config.Attr as A
import qualified Contemplata.Config.Entity.Type as E


-- | Configuration of an entity annotation, which can be assigned to a syntactic
-- node, to a relation between nodes, etc.
data Entity = Entity
  { name :: Text
    -- ^ Name of the annotation entity (e.g., Timex, Event, etc.)
  , typ :: E.EntityType
    -- ^ Entity's type: its possible values, default value, etc.
  , attributes :: [(Text, A.Attr)]
    -- ^ General attributes: their names and the configurations of their values
    -- (closed, free, anchors, ...)
  , attributesOnType :: M.Map Text [(Text, A.Attr)]
    -- ^ Type-dependent attributes
  } deriving (Generic, Show)

instance Interpret Entity

instance JSON.FromJSON Entity
instance JSON.ToJSON Entity where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


------------------------------
-- Map
------------------------------


data MapPair a = MapPair
  { key :: Text
  , val :: a
  } deriving (Generic, Show)

instance Interpret a => Interpret (MapPair a)

toPair :: MapPair a -> (Text, a)
toPair MapPair{..} = (key, val)

instance Interpret a => Interpret (M.Map Text a) where
    autoWith = fmap
      (fmap $ M.fromList . map toPair . V.toList)
      autoWith
