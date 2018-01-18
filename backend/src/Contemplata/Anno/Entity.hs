{-# LANGUAGE DeriveGeneric #-}


-- | Annotation: entities


module Contemplata.Anno.Entity
( Entity(..)
) where


import GHC.Generics (Generic)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import Data.Text (Text)

import qualified Contemplata.Anno.Attr as A


-- | An annotation entity, corresponding to `Contemplata.Config.Entity`
data Entity = Entity
  { name :: Text
    -- ^ A name of the entity
  , typ :: Text
    -- ^ Entity's type
  , attributes :: M.Map Text A.Attr
    -- ^ The value of optional attributes does not have to be specified in the
    -- map above.
  } deriving (Generic, Show, Eq, Ord)


instance JSON.FromJSON Entity
instance JSON.ToJSON Entity where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
