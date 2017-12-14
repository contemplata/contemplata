{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Attribute.


module Odil.Config.Attr
( Attr(..)
) where


import Dhall

-- | A free attribute.
data Attr
  = Closed
    { among :: Vector Text
    , def :: Maybe Text }
  | Free
    { def :: Maybe Text }
  | Anchor
  deriving (Generic, Show)

instance Interpret Attr
