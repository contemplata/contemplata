{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Odil.Anno
( Config(..)
) where


import Dhall

import qualified Odil.Anno.Entity as E


-- | A closed attribute.
data Config = Config
  { entities :: Vector E.Entity
  } deriving (Generic, Show)

instance Interpret Config
