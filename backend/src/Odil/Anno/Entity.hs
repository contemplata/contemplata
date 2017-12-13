{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Odil.Anno.Entity
( Entity(..)
) where


import Dhall

import qualified Odil.Anno.Attr as A
import qualified Odil.Anno.Entity.Type as E


data Entity = Entity
  { name :: Text
  , typ :: E.EntityType
  , attributes :: Vector A.Attr
  } deriving (Generic, Show)

instance Interpret Entity
