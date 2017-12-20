{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}


module Odil.Config.Command
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
  { keyCode :: Int
    -- ^ Key code corresponding to the `char`
  , char :: Char
    -- ^ The character of the shortcut
  } deriving (Generic, Show, Eq, Ord)

instance Interpret KeyboardShortcut
instance JSON.FromJSON KeyboardShortcut
instance JSON.ToJSON KeyboardShortcut where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | A command in the command line mode
type LineCommand = Text


-- | A menu command
type MenuCommand = Text


-- | A specification of a menu command, which can be used to invoke a particular message.
data Command = Command
  { keyCmd :: Maybe KeyboardShortcut
  , lineCmd :: Maybe LineCommand
  , menuCmd :: Maybe MenuCommand
  , withCtrl :: Maybe Bool
    -- ^ If the command has to be invoked with CTRL pressed; if `Nothing`, it
    -- can be invoked whether CTRl is pressed or not; applies to `keyCmd` and
    -- `menuCmd`
  , help :: Maybe Text
    -- ^ Just a help string
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
    extract (Dhall.Core.TextLit t) =
      case LT.uncons (Data.Text.Lazy.Builder.toLazyText t) of
        Just (c, "") -> pure c
        Nothing -> empty
    extract _ =
      empty

    expected = Dhall.Core.Text
