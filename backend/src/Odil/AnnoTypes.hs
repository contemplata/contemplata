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
, EventMod(..)

-- * Signal-related
, Signal(..)
, SignalType(..)

-- * Timex-related
, Timex(..)
, TimexCalendar(..)
, TimexFunctionInDocument(..)
, TimexType(..)
, TimexTemporalFunction(..)
, TimexMod(..)

-- -- * General
-- , Confidence(..)
) where


import GHC.Generics

import qualified Data.Text as T
import qualified Data.Aeson as JSON


--------------------------------------
--------------------------------------
-- Events
--------------------------------------
--------------------------------------


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
  , evCardinality :: T.Text
  , evMod :: Maybe EventMod
  , evPred :: T.Text
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


-- | An event's modifier(?).
data EventMod
  = Start
  | Mid
  | End
  deriving (Generic,Show,Eq,Ord)


-- -- | An event's polarity.
-- data Confidence
--     = High
--     | Medium
--     | Low
--     deriving (Generic,Show,Eq,Ord)


--------------------------------------
-- JSON
--------------------------------------


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

instance JSON.FromJSON EventMod
instance JSON.ToJSON EventMod where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- instance JSON.FromJSON Confidence
-- instance JSON.ToJSON Confidence where
--   toEncoding = JSON.genericToEncoding JSON.defaultOptions


--------------------------------------
--------------------------------------
-- Signals
--------------------------------------
--------------------------------------


-- | Information specific to an event.
data Signal = Signal
  { siType :: SignalType
  } deriving (Generic,Show,Eq,Ord)


-- | A type of an event.
data SignalType
  = Locative
  | Measure
  | Boundary
  | Orientation
  deriving (Generic,Show,Eq,Ord)


--------------------------------------
-- JSON
--------------------------------------


instance JSON.FromJSON Signal
instance JSON.ToJSON Signal where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON SignalType
instance JSON.ToJSON SignalType where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


--------------------------------------
--------------------------------------
-- Timex
--------------------------------------
--------------------------------------


-- | Information specific to a timex.
data Timex = Timex
  { tiCalendar :: TimexCalendar
  , tiPred :: T.Text
  , tiFunctionInDocument :: Maybe TimexFunctionInDocument
  , tiType :: TimexType
  , tiTemporalFunction :: Maybe TimexTemporalFunction
  , tiLingValue :: T.Text
  , tiValue :: T.Text
  , tiMod :: TimexMod
  } deriving (Generic,Show,Eq,Ord)


data TimexCalendar
  = Gregor
  | ISO -- Default
  | Jour
  | Unix
  | RD
  | Hebr
  | Islam
  | MayaCirc
  | MayaLon
  | Pers
  | JulJD
  | JulAN
  | JulMOD
  | Copt
  | Ethiop
  | Egypt
  | Armen
  | Ind
  | HindSol
  | HindSolAN
  | HindLun
  | HindLunAN
  | Chinv
  | Roman
  | Revolut
  | Postitiv
  | Balines
  | BahaiOcc
  | BahaiFut
  | Other
  deriving (Generic,Show,Eq,Ord)


data TimexFunctionInDocument
  = CreationTime
  | ExpirationTime
  | ModificationTime
  | PublicationTime
  | ReleaseTime
  | ReceptionTime
  deriving (Generic,Show,Eq,Ord)


-- | A type of an event.
data TimexType
  = Date
  | Time -- Default TIME
  | Duration
  | Set
  deriving (Generic,Show,Eq,Ord)


data TimexTemporalFunction
  = S
  | R
  deriving (Generic,Show,Eq,Ord)


data TimexMod
  = Before
  | After
  | OnOrBefore
  | OnOrAfter
  | LessThan
  | MoreThan
  | EqualOrLess
  | EqualOrMore
  | StartX -- X to distinguish from Event modifier...
  | MidX
  | EndX
  | Approx
  deriving (Generic,Show,Eq,Ord)


--------------------------------------
-- JSON
--------------------------------------


instance JSON.FromJSON Timex
instance JSON.ToJSON Timex where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON TimexCalendar
instance JSON.ToJSON TimexCalendar where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON TimexFunctionInDocument
instance JSON.ToJSON TimexFunctionInDocument where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON TimexType
instance JSON.ToJSON TimexType where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON TimexTemporalFunction
instance JSON.ToJSON TimexTemporalFunction where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.FromJSON TimexMod
instance JSON.ToJSON TimexMod where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
