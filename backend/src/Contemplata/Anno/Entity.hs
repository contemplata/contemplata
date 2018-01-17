{-# LANGUAGE DeriveGeneric #-}


module Contemplata.Anno.Entity
( Entity(..)
) where


import GHC.Generics (Generic)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import Data.Text (Text)

import qualified Contemplata.Anno.Attr as A


-- | Corresponding to `Contemplata.Config.Entity`.
data Entity = Entity
  { name :: Text
  , typ :: Text
  , attributes :: M.Map Text A.Attr
    -- ^ The value of optional attributes does not have to be specified in the
    -- map above.
  } deriving (Generic, Show, Eq, Ord)


instance JSON.FromJSON Entity
instance JSON.ToJSON Entity where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
