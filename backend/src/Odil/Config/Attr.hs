{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Attribute.


module Odil.Config.Attr
( Attr(..)
) where


import Dhall

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
