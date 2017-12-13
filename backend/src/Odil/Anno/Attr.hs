{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Attribute.


module Odil.Anno.Attr
( Attr(..)
) where


import Dhall

import qualified Odil.Anno.Attr.Closed as C
import qualified Odil.Anno.Attr.Free as F
import qualified Odil.Anno.Attr.Anchor as A


-- | A free attribute.
data Attr
  = Closed C.Closed
  | Free F.Free
  | Anchor A.Anchor
  deriving (Generic, Show)

instance Interpret Attr
