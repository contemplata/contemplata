{-# LANGUAGE DeriveGeneric #-}


-- | Fundamental annotation types.


module Odil.AnnoTypes
( Event(..)
, EventClass(..)
, EventTense(..)
) where


import GHC.Generics

import qualified Data.Aeson as JSON


-- | Information specific to an event.
data Event = Event
  { evClass :: EventClass
  , evTense :: Maybe EventTense
  } deriving (Generic,Show,Eq,Ord)


-- | A class of an event.
data EventClass
    = Aspectual
    | Cause
    | EventContainer
    | IAction
    | IState
    | Modal
    | Occurrence
    | Perception
    | Reporting
    | State
    deriving (Generic,Show,Eq,Ord)


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
data EventTense
    = Future
    | Past
    | Present
    | Imperfect
    deriving (Generic,Show,Eq,Ord)


-----------
-- JSON
-----------


instance JSON.FromJSON Event
instance JSON.ToJSON Event where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventClass
instance JSON.ToJSON EventClass where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventTense
instance JSON.ToJSON EventTense where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
