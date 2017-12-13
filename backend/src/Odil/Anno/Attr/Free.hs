{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Free attribute.


module Odil.Anno.Attr.Free
( Free(..)
) where


import Dhall


-- | A free attribute.
data Free = Free
  { name :: Text
  , def :: Text
  } deriving (Generic, Show)

instance Interpret Free
