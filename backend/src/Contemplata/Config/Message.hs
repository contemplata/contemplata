{-# LANGUAGE DeriveGeneric     #-}


module Contemplata.Config.Message
( Message(..)
) where


import Dhall
import qualified Data.Aeson as JSON


data Message
  = Simple
    { name :: Text }
    -- ^ A simple Elm message, without any arguments
  | OneArg
    { name :: Text
    , arg1 :: Text }
    -- ^ One-argument Elm message
  deriving (Generic, Show)

instance Interpret Message

instance JSON.FromJSON Message
instance JSON.ToJSON Message where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
