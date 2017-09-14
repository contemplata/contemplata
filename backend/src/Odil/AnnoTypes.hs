{-# LANGUAGE DeriveGeneric #-}


-- | Fundamental annotation types.


module Odil.AnnoTypes
(
-- * Event-related
  Event(..)
, EventClass(..)
, EventType(..)
, EventInquisit(..)
, EventTime(..)
, EventAspect(..)
, EventPolarity(..)
, EventModality(..)

-- -- * General
-- , Confidence(..)
) where


import GHC.Generics

import qualified Data.Text as T
import qualified Data.Aeson as JSON


-- | Information specific to an event.
data Event = Event
  { evClass :: EventClass
  , evType :: EventType
  , evInquisit :: EventInquisit
  , evTime :: Maybe EventTime
  , evAspect :: Maybe EventAspect
  , evPolarity :: EventPolarity
  , evMood :: Maybe EventMood
  , evModality :: Maybe EventModality
  , evComment :: T.Text
    -- ^ Optional comment left by the annotator
--   , evConfidence :: Confidence
--     -- ^ Confidence of the annotator w.r.t. to this particular event annotation
  } deriving (Generic,Show,Eq,Ord)


-- | A class of an event.
data EventClass
    = Occurrence -- ^ Default
    | Perception
    | Reporting
    | Aspectual
    | State
    | IState
    | IAction
    | Cause
    | EventContainer
--     | Modal
    deriving (Generic,Show,Eq,Ord)


-- | A type of an event.
data EventType
    = StateT -- using T suffix because `EventClass` also contains the value `State`
    | Process -- default
    | Transition
    deriving (Generic,Show,Eq,Ord)


data EventInquisit
  = TQuest
  | Quest
  | Order
  | TOrder
  | Decl -- default
  deriving (Generic,Show,Eq,Ord)


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
data EventTime
    = Present -- default
    | Past
    | Future
    | Omni
    | Zero
    --- | Imperfect
    deriving (Generic,Show,Eq,Ord)


-- -- | An event's aspect. The default is `None`, which should be represented by
-- -- `Nothing`.
-- data EventAspect
--     = Progressive
--     | Perfective
--     | Imperfective
--     | PerfectiveProgressive
--     | ImperfectiveProgressive
--     deriving (Generic,Show,Eq,Ord)


-- | An event's aspect. The default is `None`, which should be represented by
-- `Nothing`.
data EventAspect
    = Progressive
    | Perfective
    | Prospective
    | Imperfective
    --- | PerfectiveProgressive
    --- | ImperfectiveProgressive
    deriving (Generic,Show,Eq,Ord)


-- | An event's polarity.
data EventPolarity
    = Pos
    | Neg
    deriving (Generic,Show,Eq,Ord)


-- | An event's mood.
data EventMood
    = Subjunctive
    | Conditional
    deriving (Generic,Show,Eq,Ord)


-- | An event's modality.
data EventModality
  = Certainty
  | Conjectural
  | Necessity
  | Obligation
  | Permission
  | Possibility
  | Probability
  deriving (Generic,Show,Eq,Ord)


-- -- | An event's polarity.
-- data Confidence
--     = High
--     | Medium
--     | Low
--     deriving (Generic,Show,Eq,Ord)


-----------
-- JSON
-----------


instance JSON.FromJSON Event
instance JSON.ToJSON Event where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventClass
instance JSON.ToJSON EventClass where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventType
instance JSON.ToJSON EventType where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventInquisit
instance JSON.ToJSON EventInquisit where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventTime
instance JSON.ToJSON EventTime where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventAspect
instance JSON.ToJSON EventAspect where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventPolarity
instance JSON.ToJSON EventPolarity where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventMood
instance JSON.ToJSON EventMood where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON EventModality
instance JSON.ToJSON EventModality where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- instance JSON.FromJSON Confidence
-- instance JSON.ToJSON Confidence where
--   toEncoding = JSON.genericToEncoding JSON.defaultOptions
