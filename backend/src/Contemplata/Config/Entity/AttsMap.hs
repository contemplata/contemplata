{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Attribute map, currently encoded as a list of (name, value) pairs.
-- This is a bit of a hack, we could use a map (dictionary), too.
-- But that would require additional changes on the front-end side.


module Contemplata.Config.Entity.AttsMap
( MapPair(..)
, AttsMap(..)
) where


import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Dhall
import qualified Data.Aeson as JSON

import qualified Contemplata.Config.Attr as A


--------------------------
-- Map pair
--------------------------


data MapPair a = MapPair
  { typ :: Text
  , attributes :: a
  } deriving (Generic, Show)

instance Interpret a => Interpret (MapPair a)

toPair :: MapPair a -> (Text, a)
toPair MapPair{..} = (typ, attributes)


--------------------------
-- Attributes on type map
--------------------------


newtype AttsMap a = AttsMap {unAttsMap :: M.Map Text a}
  deriving (Show, Eq, Ord, JSON.FromJSON, JSON.ToJSON)

instance Interpret a => Interpret (AttsMap a) where
    autoWith = fmap
      (fmap $ AttsMap . M.fromList . map toPair . V.toList)
      autoWith
