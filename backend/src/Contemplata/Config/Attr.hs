{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Annotation configuration: attributes


module Contemplata.Config.Attr
( Attr(..)
) where


import Dhall
import qualified Data.Aeson as JSON

-- | An attribute.
data Attr
  = Closed
    { among :: Vector Text
    , def :: Maybe Text
    , required :: Bool }
  | Free
    { def :: Maybe Text }
  | Anchor
  deriving (Generic, Show)

instance Interpret Attr

instance JSON.FromJSON Attr
instance JSON.ToJSON Attr where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
