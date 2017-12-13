{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Closed attribute.


module Odil.Anno.Attr.Closed
( Closed(..)
) where


import Dhall


-- | A closed attribute.
data Closed = Closed
  { name :: Text
  , among :: Vector Text
  , def :: Maybe Text
  } deriving (Generic, Show)

instance Interpret Closed
