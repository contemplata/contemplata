{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}


module Contemplata.Config.Command
( Command(..)
) where


import Control.Applicative (empty, pure)

import qualified Data.Aeson as JSON
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder

import Dhall
import qualified Dhall.Core


-- | Command invocation via a keyboard shortcut
data KeyboardShortcut = KeyboardShortcut
  { char :: Char
    -- ^ The character of the shortcut
  } deriving (Generic, Show, Eq, Ord)

instance Interpret KeyboardShortcut
instance JSON.FromJSON KeyboardShortcut
instance JSON.ToJSON KeyboardShortcut where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Command invocation via the command-line
type LineCommand = Text


-- | Command invocation via menu item
type MenuCommand = Text


-- | A specification of a Contemplata command, which can be used to invoke a
-- particular `Contemplata.Config.Message`.
data Command = Command
  { keyCmd :: Maybe KeyboardShortcut
  , lineCmd :: Maybe LineCommand
  , menuCmd :: Maybe MenuCommand
  , withCtrl :: Maybe Bool
    -- ^ If the command has to be invoked with CTRL pressed. Relates to `keyCmd`
    -- and `menuCmd`, but not to `lineCmd`. If `Nothing`, the command can be
    -- invoked whether CTRl is pressed or not; applies to `keyCmd` and `menuCmd`
  , help :: Maybe Text
    -- ^ The corresponding help message
  } deriving (Generic, Show, Eq, Ord)

instance Interpret Command
instance JSON.FromJSON Command
instance JSON.ToJSON Command where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


------------------------------
-- Intepret instances
------------------------------


instance Interpret Int where
  autoWith =
    fmap (fromIntegral :: Integer -> Int) . autoWith

instance Interpret Char where
  autoWith _ = dhallChar

dhallChar :: Type Char
dhallChar = Type {..}
  where
--     extract (Dhall.Core.TextLit t) =
--       case LT.uncons (Data.Text.Lazy.Builder.toLazyText t) of
--         Just (c, "") -> pure c
--         _ -> empty
    extract (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) =
      case T.unpack t of
        [c] -> pure c
        _ -> empty
    extract _ =
      empty

    expected = Dhall.Core.Text
