{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module Odil.Config.Entity
( Entity(..)
) where


import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Dhall
import qualified Data.Aeson as JSON

import qualified Odil.Config.Attr as A
import qualified Odil.Config.Entity.Type as E


data Entity = Entity
  { name :: Text
  , typ :: E.EntityType
  -- , attributes :: M.Map Text A.Attr
  , attributes :: [(Text, A.Attr)]
  , attributesOnType :: M.Map Text [(Text, A.Attr)]
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
