{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Attribute map, currently encoded as a list of (name, value) pairs.
-- This is a bit of a hack, we could use a map (dictionary), too.
-- But that would require additional changes on the front-end side.


module Contemplata.Config.Entity.AttrMap
( AttrMap(..)
, AttrPair(..)
) where


import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Dhall
import qualified Data.Aeson as JSON

import qualified Contemplata.Config.Attr as A


data AttrPair a = AttrPair
  { name :: Text
  , value :: a
  } deriving (Generic, Show)

instance Interpret a => Interpret (AttrPair a)

toPair :: AttrPair a -> (Text, a)
toPair AttrPair{..} = (name, value)

newtype AttrMap a = AttrMap {unAttrMap :: [(Text, a)]}
  deriving (Show, Eq, Ord, JSON.FromJSON, JSON.ToJSON)

instance Interpret a => Interpret (AttrMap a) where
    autoWith = fmap
      (fmap $ AttrMap . M.toList . M.fromList . map toPair . V.toList)
      autoWith

-- type AttrMap a = M.Map Text a
-- 
-- instance Interpret a => Interpret (AttrMap a) where
--     autoWith = fmap
--       (fmap $ M.fromList . map toPair . V.toList)
--       autoWith
