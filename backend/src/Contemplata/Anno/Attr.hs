{-# LANGUAGE DeriveGeneric #-}


module Contemplata.Anno.Attr
( Attr(..)
) where

import GHC.Generics (Generic)

import qualified Data.Aeson as JSON
import Data.Text (Text)

import Contemplata.Types.Core (Addr)


-- | Corresponding to `Contemplata.Config.Attr`.
data Attr
  = Attr Text
    -- ^ A closed or free attribute.
  | Anchor Addr
  deriving (Generic, Show, Eq, Ord)


instance JSON.FromJSON Attr
instance JSON.ToJSON Attr where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
