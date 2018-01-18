{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Annotation configuration: attributes


module Contemplata.Config.Attr
( Attr(..)
) where


import Dhall
import qualified Data.Aeson as JSON

-- | An attribute can be either:
-- * Closed: the set of its values is fixed
-- * Free: any textual value is acceptable
-- * Anchor: a pointer to anoter node (possibly in a different tree)
data Attr
  = Closed
    { among :: Vector Text
      -- ^ The set of values to which the value of the attribute must belong
    , def :: Maybe Text
      -- ^ Default value (if any); doesn't have to be specified if not
      -- `required`
    , required :: Bool
      -- ^ Is the attribute required or not?
    }
  | Free
    { def :: Maybe Text
      -- ^ Default value (if any)
    }
  | Anchor
  deriving (Generic, Show)

instance Interpret Attr

instance JSON.FromJSON Attr
instance JSON.ToJSON Attr where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
