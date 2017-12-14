{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Fundamental annotation types, configured via Dhall.


module Odil.Config
( Config(..)
) where


import Dhall

import qualified Odil.Config.Entity as E


-- | A closed attribute.
data Config = Config
  { entities :: Vector E.Entity
  } deriving (Generic, Show)

instance Interpret Config
