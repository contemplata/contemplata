{-# LANGUAGE DeriveGeneric #-}


-- | Annotation: attributes


module Contemplata.Anno.Attr
( Attr(..)
) where

import GHC.Generics (Generic)

import qualified Data.Aeson as JSON
import Data.Text (Text)

import Contemplata.Types.Core (Addr)


-- | An attribute value, consistent with `Contemplata.Config.Attr`
data Attr
  = Attr Text
    -- ^ A closed or free attribute.
  | Anchor Addr
    -- ^ An anchor (pointer to a tree node)
  deriving (Generic, Show, Eq, Ord)


instance JSON.FromJSON Attr
instance JSON.ToJSON Attr where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
