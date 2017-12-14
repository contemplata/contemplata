{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Anchor attribute.


module Odil.Anno.Attr.Anchor
( Anchor(..)
) where


import Dhall


-- | An anchor attribute.
data Anchor = Anchor
  deriving (Generic, Show)

instance Interpret Anchor
