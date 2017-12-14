{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Attribute.


module Odil.Anno.Attr
( Attr(..)
) where


import Dhall

-- import qualified Odil.Anno.Attr.Closed as C
-- import qualified Odil.Anno.Attr.Free as F
-- import qualified Odil.Anno.Attr.Anchor as A


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
