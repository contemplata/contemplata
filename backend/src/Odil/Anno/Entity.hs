{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module Odil.Anno.Entity
( Entity(..)
) where


import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Dhall

import qualified Odil.Anno.Attr as A
import qualified Odil.Anno.Entity.Type as E


data Entity = Entity
  { name :: Text
  , typ :: E.EntityType
  , attributes :: M.Map Text A.Attr
  } deriving (Generic, Show)

instance Interpret Entity


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
