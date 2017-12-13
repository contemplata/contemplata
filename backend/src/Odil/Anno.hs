{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Odil.Anno
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
