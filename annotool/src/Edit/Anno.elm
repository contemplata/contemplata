-- | Fundamental annotation-related types.


module Edit.Anno exposing
  (
  -- * Nodes (in general)
    nodeLabelSet
  , NodeAttr (..)

  -- * Events
  , Event(..)
  , EventAttr (..)

  , EventType(..)
  , EventInquisit(..)
  , EventClass(..)
  , EventTime(..)
  , EventAspect(..)
  , EventPolarity(..)
  , EventMood(..)
  , EventModality(..)
  , EventMod(..)
  , eventDefault
  , eventClassStr
  -- , eventClassFromStr
  , eventTypeStr
  , eventInquisitStr
  , eventTimeStr
  -- , eventTimeFromStr
  , eventAspectStr
  , eventPolarityStr
  , eventMoodStr
  , eventModalityStr
  , eventModStr

  -- ** JSON
  , encodeEvent
  , eventDecoder

  -- ** Utils
  , nullable
  , valueFromStr

  -- * Signals
  , Signal(..)
  , SignalAttr(..)
  , SignalType(..)
  , signalDefault
  , signalTypeStr

  -- ** JSON
  , encodeSignal
  , signalDecoder

  -- * Timex
  , Timex(..)
  , TimexAttr(..)
  , TimexCalendar(..)
  , TimexFunctionInDocument(..)
  , TimexType(..)
  , TimexTemporalFunction(..)
  , TimexMod(..)
  , timexDefault
  , timexTypeStr
  , timexCalendarStr
  , timexFunctionInDocumentStr
  , timexTemporalFunctionStr
  , timexModStr

  -- ** JSON
  , encodeTimex
  , timexDecoder
  )


import Json.Decode as Decode
import Json.Encode as Encode

import Set as S
import Dict as D

import Edit.Core exposing (..)
import Util


---------------------------------------------------
---------------------------------------------------
-- General
---------------------------------------------------
---------------------------------------------------


---------------------------------------------------
-- Label
---------------------------------------------------


nodeLabelSet : S.Set String
nodeLabelSet = S.fromList
  [ "ADJ"
  , "ADJWH"
  , "ADV"
  , "ADVWH"
  , "AP"
  , "AdP"
  , "C"
  , "CC"
  , "CL"
  , "CLO"
  , "CLR"
  , "CLS"
  , "COORD"
  , "CS"
  , "DET"
  , "DETWH"
  , "ET"
  , "I"
  , "MWA"
  , "MWADV"
  , "MWC"
  , "MWCL"
  , "MWD"
  , "MWN"
  , "MWP"
  , "MWPRO"
  , "MWV"
  , "N"
  , "NC"
  , "NP"
  , "NP"
  , "P"
  , "PP"
  , "PREF"
  , "PRO"
  , "PROREL"
  , "PROWH"
  , "PUNC"
  , "ROOT"
  , "SENT"
  , "Sint"
  , "Srel"
  , "Ssub"
  , "V"
  , "VIMP"
  , "VINF"
  , "VN"
  , "VP"
  , "VPP"
  , "VPR"
  , "VPinf"
  , "VPpart"
  , "VS"
  -- below, custom additional labels
  , "PARA_COORD" ]


type NodeAttr
    = NodeLabelAttr String
    | NodeCommentAttr String


---------------------------------------------------
---------------------------------------------------
-- Event
---------------------------------------------------
---------------------------------------------------


-- | Information specific to an event.
type Event = Event
  { evClass : EventClass
  , evType : EventType
  , evInquisit : EventInquisit
  , evTime : Maybe EventTime
  , evAspect : Maybe EventAspect
  , evPolarity : EventPolarity
  , evMood : Maybe EventMood
  , evModality : Maybe EventModality
  , evCardinality : String
  , evMod : Maybe EventMod
  , evPred : String
  -- , evComment : String
  -- , evConfidence :: Confidence
  }


eventDefault : Event
eventDefault = Event
  { evClass = Occurrence
  , evType = Process
  , evInquisit = Decl
  , evTime = Just Present
  , evAspect = Nothing
  , evPolarity = Pos
  , evMood = Nothing
  , evModality = Nothing
  , evCardinality = ""
  , evMod = Nothing
  , evPred = ""
  -- , evComment = ""
  }


type EventAttr
    = ClassAttr EventClass
    | TypeAttr EventType
    | InquisitAttr EventInquisit
    | TimeAttr (Maybe EventTime)
    | AspectAttr (Maybe EventAspect)
    | PolarityAttr EventPolarity
    | MoodAttr (Maybe EventMood)
    | ModalityAttr (Maybe EventModality)
    | CardinalityAttr String
    | ModAttr (Maybe EventMod)
    | PredAttr String
    -- | CommentAttr String


---------------------------------------------------
-- Class
---------------------------------------------------


-- | A class of an event.
type EventClass
    = Occurrence
    | Perception
    | Reporting
    | Aspectual
    | State
    | IState
    | IAction
    | Cause
    | EventContainer
--     | Modal


-- | The string representations of an event class.
eventClassStr : List (String, EventClass)
eventClassStr =
  [ ("Aspectual", Aspectual)
  , ("IAction", IAction)
  , ("IState", IState)
  , ("Occurrence", Occurrence)
  , ("Perception", Perception)
  , ("Reporting", Reporting)
  , ("State", State) ]
  -- , ("Cause", Cause)
  -- , ("EventContainer", EventContainer)
  -- , ("Modal", Modal)


valueFromStr : List (String, a) -> String -> a
valueFromStr lst x =
  let
      d = D.fromList lst
  in
      case D.get x d of
          Nothing -> Debug.crash "Anno.valueFromStr: cannot decode"
          Just y -> y


strFromValue : List (String, a) -> a -> String
strFromValue lst val =
  let
      go all = case all of
        [] -> Debug.crash "Anno.strFromValue: cannot decode"
        ((x, y) :: xs) ->
          if y == val
          then x
          else go xs
  in
      go lst


valueFromStrSafe : List (String, a) -> String -> Maybe a
valueFromStrSafe lst x =
  let d = D.fromList lst
  in  D.get x d


nullable : List (String, a) -> List (String, Maybe a)
nullable xs =
  let just (x, y) = (x, Just y)
  in  ("--", Nothing) :: List.map just xs


eventClassFromStr : String -> EventClass
eventClassFromStr = valueFromStr eventClassStr
--  let
--      d = D.fromList eventClassStr
--  in
--      case D.get x d of
--          Nothing -> Debug.crash "Anno.eventClassFromStr: cannot decode"
--          Just y -> y


---------------------------------------------------
-- Type
---------------------------------------------------


-- | A type of an event.
type EventType
    = StateT
    | Process
    | Transition


-- | The string representations of an event class.
eventTypeStr : List (String, EventType)
eventTypeStr =
  [ ("StateT", StateT)
  , ("Process", Process)
  , ("Transition", Transition) ]


---------------------------------------------------
-- Inquisit
---------------------------------------------------


-- | A type of an event.
type EventInquisit
    = TQuest
    | Quest
    | Order
    | TOrder
    | Decl -- default


-- | The string representations of an event class.
eventInquisitStr : List (String, EventInquisit)
eventInquisitStr =
  [ ("TQuest", TQuest)
  , ("Quest", Quest)
  , ("Order", Order)
  , ("TOrder", TOrder)
  , ("Decl", Decl) ]


---------------------------------------------------
-- Time
---------------------------------------------------


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
type EventTime
    = Future
    | Past
    | Present
    | Omni
    | Zero
    -- | Imperfect


-- | The string representations of an event class.
eventTimeStr : List (String, EventTime)
eventTimeStr =
  [ ("Future", Future)
  , ("Past", Past)
  , ("Present", Present)
  -- , ("Imperfect", Imperfect) ]
  , ("Omni", Omni)
  , ("Zero", Zero) ]


eventTimeFromStr : String -> Maybe EventTime
eventTimeFromStr = valueFromStrSafe eventTimeStr
--   case D.get x (D.fromList eventTimeStr) of
--     Nothing -> Nothing
--     Just y -> y


---------------------------------------------------
-- Aspect
---------------------------------------------------


-- | An event's aspect. The default is `None`, which should be represented by
-- `Nothing`.
type EventAspect
    = Progressive
    | Perfective
    | Prospective
    | Imperfective
    -- | PerfectiveProgressive
    -- | ImperfectiveProgressive


-- | The string representations of an event class.
eventAspectStr : List (String, EventAspect)
eventAspectStr =
  [ ("Progressive", Progressive)
  , ("Perfective", Perfective)
  , ("Prospective", Prospective)
  , ("Imperfective", Imperfective) ]
  -- , ("PerfectiveProgressive", PerfectiveProgressive)
  -- , ("ImperfectiveProgressive", ImperfectiveProgressive) ]


---------------------------------------------------
-- Polarity
---------------------------------------------------

-- | An event's polarity.
type EventPolarity
    = Pos
    | Neg


-- | The string representations of an event class.
eventPolarityStr : List (String, EventPolarity)
eventPolarityStr =
  [ ("Pos", Pos)
  , ("Neg", Neg) ]


---------------------------------------------------
-- Subjunctive mood
---------------------------------------------------

-- | An event's mood.
type EventMood
    = Subjunctive
    | Conditional


-- | The string representations of an event class.
eventMoodStr : List (String, EventMood)
eventMoodStr =
  [ ("Subjunctive", Subjunctive)
  , ("Conditional", Conditional) ]


---------------------------------------------------
-- Modality
---------------------------------------------------


-- | An event's modality.
type EventModality
    = Certainty
    | Conjectural
    | Necessity
    | Obligation
    | Permission
    | Possibility
    | Probability


-- | The string representations of an event class.
eventModalityStr : List (String, EventModality)
eventModalityStr =
  [ ("Certainty", Certainty)
  , ("Conjectural", Conjectural)
  , ("Necessity", Necessity)
  , ("Obligation", Obligation)
  , ("Permission", Permission)
  , ("Possibility", Possibility)
  , ("Probability", Probability) ]


---------------------------------------------------
-- Mod(ifier?)
---------------------------------------------------


-- | An event's mod(ifier?).
type EventMod
  = Start
  | Mid
  | End


-- | The string representations.
eventModStr : List (String, EventMod)
eventModStr =
  [ ("Start", Start)
  , ("Mid", Mid)
  , ("End", End)
  ]


-- ---------------------------------------------------
-- -- Confidence
-- ---------------------------------------------------
--
--
-- -- | An event's polarity.
-- type Confidence
--     = High
--     | Medium
--     | Low
--
--
-- -- | The string representations of an event class.
-- eventConfidenceStr : List (String, Confidence)
-- eventConfidenceStr =
--   [ ("High", High)
--   , ("Medium", Medium)
--   , ("Low", Low) ]


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


eventDecoder : Decode.Decoder Event
eventDecoder =
  let mkEvent cls typ inq tns asp pol subj modal car mod pred = -- com =
        Event
        { evClass=cls
        , evType=typ
        , evInquisit=inq
        , evTime=tns
        , evAspect=asp
        , evPolarity=pol
        , evMood=subj
        , evModality=modal
        , evCardinality=car
        , evMod=mod
        , evPred=pred
        -- , evComment=com
        }
  in  decodeMap11 mkEvent
        (Decode.field "evClass" eventClassDecoder)
        (Decode.field "evType" eventTypeDecoder)
        (Decode.field "evInquisit" eventInquisitDecoder)
        (Decode.field "evTime" (Decode.nullable eventTimeDecoder))
        (Decode.field "evAspect" (Decode.nullable eventAspectDecoder))
        (Decode.field "evPolarity" eventPolarityDecoder)
        (Decode.field "evMood" (Decode.nullable eventMoodDecoder))
        (Decode.field "evModality" (Decode.nullable eventModalityDecoder))
        (Decode.field "evCardinality" Decode.string)
        (Decode.field "evMod" (Decode.nullable eventModDecoder))
        (Decode.field "evPred" Decode.string)
        -- (Decode.field "evComment" Decode.string)


eventClassDecoder : Decode.Decoder EventClass
eventClassDecoder =
  let
    decode = valueFromStr eventClassStr
--     decode x = case x of
--         "Aspectual" -> Aspectual
--         "Cause" -> Cause
--         "EventContainer" -> EventContainer
--         "IAction" -> IAction
--         "IState" -> IState
--         "Modal" -> Modal
--         "Occurrence" -> Occurrence
--         "Perception" -> Perception
--         "Reporting" -> Reporting
--         "State" -> State
--         _ -> Debug.crash ("Anno.eventClassDecoder -- unknown value: " ++ x)
  in

      Decode.map decode Decode.string


eventTimeDecoder : Decode.Decoder EventTime
eventTimeDecoder =
  let
    decode = valueFromStr eventTimeStr
--     decode x = case x of
--       "Future" -> Future
--       "Past" -> Past
--       "Present" -> Present
--       "Imperfect" -> Imperfect
--       _ -> Debug.crash ("Anno.eventTimeDecoder -- unknown value: " ++ x)
  in
    Decode.map decode Decode.string


attrDecoder : List (String, a) -> Decode.Decoder a
attrDecoder lst =
  let decode = valueFromStr lst
  in  Decode.map decode Decode.string


eventTypeDecoder : Decode.Decoder EventType
eventTypeDecoder = attrDecoder eventTypeStr


eventInquisitDecoder : Decode.Decoder EventInquisit
eventInquisitDecoder = attrDecoder eventInquisitStr


eventAspectDecoder : Decode.Decoder EventAspect
eventAspectDecoder = attrDecoder eventAspectStr


eventPolarityDecoder : Decode.Decoder EventPolarity
eventPolarityDecoder = attrDecoder eventPolarityStr


eventMoodDecoder : Decode.Decoder EventMood
eventMoodDecoder = attrDecoder eventMoodStr


eventModalityDecoder : Decode.Decoder EventModality
eventModalityDecoder = attrDecoder eventModalityStr


eventModDecoder : Decode.Decoder EventMod
eventModDecoder = attrDecoder eventModStr


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeEvent : Event -> Encode.Value
encodeEvent (Event r) = Encode.object
    [ ("evClass", encodeEventClass r.evClass)
    , ("evType", encodeEventType r.evType)
    , ("evInquisit", encodeEventInquisit r.evInquisit)
    , ("evTime", Util.encodeMaybe encodeEventTime r.evTime)
    , ("evAspect", Util.encodeMaybe encodeEventAspect r.evAspect)
    , ("evPolarity", encodeEventPolarity r.evPolarity)
    , ("evMood", Util.encodeMaybe encodeEventMood r.evMood)
    , ("evModality", Util.encodeMaybe encodeEventModality r.evModality)
    , ("evCardinality", Encode.string r.evCardinality)
    , ("evMod", Util.encodeMaybe encodeEventMod r.evMod)
    , ("evPred", Encode.string r.evPred)
    -- , ("evComment", Encode.string r.evComment)
    ]


encodeEventClass : EventClass -> Encode.Value
encodeEventClass cls = Encode.string <|
  strFromValue eventClassStr cls


encodeEventType : EventType -> Encode.Value
encodeEventType x = Encode.string <|
  strFromValue eventTypeStr x


encodeEventInquisit : EventInquisit -> Encode.Value
encodeEventInquisit x = Encode.string <|
  strFromValue eventInquisitStr x


encodeEventTime : EventTime -> Encode.Value
encodeEventTime x = Encode.string <|
  strFromValue eventTimeStr x


encodeEventAspect : EventAspect -> Encode.Value
encodeEventAspect x = Encode.string <|
  strFromValue eventAspectStr x


encodeEventPolarity : EventPolarity -> Encode.Value
encodeEventPolarity x = Encode.string <|
  strFromValue eventPolarityStr x


encodeEventMood : EventMood -> Encode.Value
encodeEventMood x = Encode.string <|
  strFromValue eventMoodStr x


encodeEventModality : EventModality -> Encode.Value
encodeEventModality x = Encode.string <|
  strFromValue eventModalityStr x


encodeEventMod : EventMod -> Encode.Value
encodeEventMod x = Encode.string <|
  strFromValue eventModStr x



---------------------------------------------------
---------------------------------------------------
-- Signals
---------------------------------------------------
---------------------------------------------------


-- | Information specific to an event.
type Signal = Signal
  { siType : SignalType
  }


signalDefault : Signal
signalDefault = Signal
  { siType = Locative
  }


type SignalAttr
    = SiTypeAttr SignalType


---------------------------------------------------
-- Type
---------------------------------------------------


-- | A type of a signal
type SignalType
    = Locative
    | Measure
    | Boundary
    | Orientation


-- | The string representations of an event class.
signalTypeStr : List (String, SignalType)
signalTypeStr =
  [ ("Locative", Locative)
  , ("Measure", Measure)
  , ("Boundary", Boundary)
  , ("Orientation", Orientation) ]


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeSignal : Signal -> Encode.Value
encodeSignal (Signal r) = Encode.object
    [ ("siType", encodeSignalType r.siType)
    ]


encodeSignalType : SignalType -> Encode.Value
encodeSignalType x = Encode.string <|
  strFromValue signalTypeStr x


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


signalDecoder : Decode.Decoder Signal
signalDecoder =
  let mkSignal typ =
        Signal
        { siType=typ
        }
  in  Decode.map mkSignal
        (Decode.field "siType" signalTypeDecoder)


signalTypeDecoder : Decode.Decoder SignalType
signalTypeDecoder = attrDecoder signalTypeStr


---------------------------------------------------
---------------------------------------------------
-- Timex
---------------------------------------------------
---------------------------------------------------


-- | Information specific to an event.
type Timex = Timex
  { tiCalendar : TimexCalendar
  , tiPred : String
  , tiFunctionInDocument : Maybe TimexFunctionInDocument
  , tiType : TimexType
  , tiTemporalFunction : Maybe TimexTemporalFunction
  , tiLingValue : String
  , tiValue : String
  , tiMod : Maybe TimexMod
  , tiAnchor : Maybe Addr
  , tiBeginPoint : Maybe Addr
  , tiEndPoint : Maybe Addr
  -- ^ Both above can be only set if `tiType == Duration`
  , tiQuant : Maybe String
  , tiFreq : Maybe String
  -- ^ Both above can be only set if `tiType == Set`
  }


timexDefault : Timex
timexDefault = Timex
  { tiCalendar = ISO
  , tiPred = ""
  , tiFunctionInDocument = Nothing
  , tiType = Time
  , tiTemporalFunction = Nothing
  , tiLingValue = ""
  , tiValue = ""
  , tiMod = Nothing
  , tiAnchor = Nothing
  , tiBeginPoint = Nothing
  , tiEndPoint = Nothing
  , tiQuant = Nothing
  , tiFreq = Nothing
  }


type TimexAttr
    = TiCalendarAttr TimexCalendar
    | TiTypeAttr TimexType
    | TiFunctionInDocumentAttr (Maybe TimexFunctionInDocument)
    | TiPredAttr String
    | TiTemporalFunctionAttr (Maybe TimexTemporalFunction)
    | TiLingValueAttr String
    | TiValueAttr String
    | TiModAttr (Maybe TimexMod)
    | TiAnchorAttr Bool -- ^ Create if `True`, remove if `False`
    | TiBeginPointAttr Bool -- ^ Create if `True`, remove if `False`
    | TiEndPointAttr Bool -- ^ Create if `True`, remove if `False`
    | TiQuantAttr (Maybe String)
    | TiFreqAttr (Maybe String)


---------------------------------------------------
-- Calendar
---------------------------------------------------


type TimexCalendar
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


-- | The string representations of an event class.
timexCalendarStr : List (String, TimexCalendar)
timexCalendarStr =
  [ ("Gregor", Gregor)
  , ("ISO", ISO)
  , ("Jour", Jour)
  , ("Unix", Unix)
  , ("RD", RD)
  , ("Hebr", Hebr)
  , ("Islam", Islam)
  , ("MayaCirc", MayaCirc)
  , ("MayaLon", MayaLon)
  , ("Pers", Pers)
  , ("JulJD", JulJD)
  , ("JulAN", JulAN)
  , ("JulMOD", JulMOD)
  , ("Copt", Copt)
  , ("Ethiop", Ethiop)
  , ("Egypt", Egypt)
  , ("Armen", Armen)
  , ("Ind", Ind)
  , ("HindSol", HindSol)
  , ("HindSolAN", HindSolAN)
  , ("HindLun", HindLun)
  , ("HindLunAN", HindLunAN)
  , ("Chinv", Chinv)
  , ("Roman", Roman)
  , ("Revolut", Revolut)
  , ("Postitiv", Postitiv)
  , ("Balines", Balines)
  , ("BahaiOcc", BahaiOcc)
  , ("BahaiFut", BahaiFut)
  , ("Other", Other) ]


---------------------------------------------------
-- FunctionInDocument
---------------------------------------------------


type TimexFunctionInDocument
  = CreationTime
  | ExpirationTime
  | ModificationTime
  | PublicationTime
  | ReleaseTime
  | ReceptionTime


timexFunctionInDocumentStr : List (String, TimexFunctionInDocument)
timexFunctionInDocumentStr =
  [ ("CreationTime", CreationTime)
  , ("ExpirationTime", ExpirationTime)
  , ("ModificationTime", ModificationTime)
  , ("PublicationTime", PublicationTime)
  , ("ReleaseTime", ReleaseTime)
  , ("ReceptionTime", ReceptionTime) ]


---------------------------------------------------
-- Type
---------------------------------------------------


type TimexType
  = Date
  | Time -- Default
  | Duration
  | Set



timexTypeStr : List (String, TimexType)
timexTypeStr =
  [ ("Date", Date)
  , ("Time", Time)
  , ("Duration", Duration)
  , ("Set", Set) ]


---------------------------------------------------
-- Temporal Function
---------------------------------------------------


type TimexTemporalFunction
  = S
  | R


timexTemporalFunctionStr : List (String, TimexTemporalFunction)
timexTemporalFunctionStr =
  [ ("S", S)
  , ("R", R) ]


---------------------------------------------------
-- Modifier
---------------------------------------------------


type TimexMod
  = Before
  | After
  | OnOrBefore
  | OnOrAfter
  | LessThan
  | MoreThan
  | EqualOrLess
  | EqualOrMore
  | StartX
  | MidX
  | EndX
  | Approx


timexModStr : List (String, TimexMod)
timexModStr =
  [ ("Before", Before)
  , ("After", After)
  , ("OnOrBefore", OnOrBefore)
  , ("OnOrAfter", OnOrAfter)
  , ("LessThan", LessThan)
  , ("MoreThan", MoreThan)
  , ("EqualOrLess", EqualOrLess)
  , ("EqualOrMore", EqualOrMore)
  , ("StartX", StartX)
  , ("MidX", MidX)
  , ("EndX", EndX)
  , ("Approx", Approx) ]


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeTimex : Timex -> Encode.Value
encodeTimex (Timex r) = Encode.object
    [ ("tiCalendar", encodeTimexCalendar r.tiCalendar)
    , ("tiPred", Encode.string r.tiPred)
    , ("tiFunctionInDocument", Util.encodeMaybe encodeTimexFunctionInDocument r.tiFunctionInDocument)
    , ("tiType", encodeTimexType r.tiType)
    , ("tiTemporalFunction", Util.encodeMaybe encodeTimexTemporalFunction r.tiTemporalFunction)
    , ("tiLingValue", Encode.string r.tiLingValue)
    , ("tiValue", Encode.string r.tiValue)
    , ("tiMod", Util.encodeMaybe encodeTimexMod r.tiMod)
    , ("tiAnchor", Util.encodeMaybe encodeAddr r.tiAnchor)
    , ("tiBeginPoint", Util.encodeMaybe encodeAddr r.tiBeginPoint)
    , ("tiEndPoint", Util.encodeMaybe encodeAddr r.tiEndPoint)
    , ("tiQuant", Util.encodeMaybe Encode.string r.tiQuant)
    , ("tiFreq", Util.encodeMaybe Encode.string r.tiFreq)
    ]


encodeTimexCalendar : TimexCalendar -> Encode.Value
encodeTimexCalendar x = Encode.string <|
  strFromValue timexCalendarStr x


encodeTimexFunctionInDocument : TimexFunctionInDocument -> Encode.Value
encodeTimexFunctionInDocument x = Encode.string <|
  strFromValue timexFunctionInDocumentStr x


encodeTimexType : TimexType -> Encode.Value
encodeTimexType x = Encode.string <|
  strFromValue timexTypeStr x


encodeTimexTemporalFunction : TimexTemporalFunction -> Encode.Value
encodeTimexTemporalFunction x = Encode.string <|
  strFromValue timexTemporalFunctionStr x


encodeTimexMod : TimexMod -> Encode.Value
encodeTimexMod x = Encode.string <|
  strFromValue timexModStr x


encodeAddr : Addr -> Encode.Value
encodeAddr (x, y) = Encode.list [Encode.int x, Encode.int y]


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


timexDecoder : Decode.Decoder Timex
timexDecoder =
  let mkTimex cal pred fun typ temp ling val mod anc beg end quant freq =
        Timex
        { tiCalendar=cal
        , tiPred = pred
        , tiFunctionInDocument = fun
        , tiType = typ
        , tiTemporalFunction = temp
        , tiLingValue = ling
        , tiValue = val
        , tiMod = mod
        , tiAnchor = anc
        , tiBeginPoint = beg
        , tiEndPoint = end
        , tiQuant = quant
        , tiFreq = freq
        }
  in  decodeMap13 mkTimex
        (Decode.field "tiCalendar" timexCalendarDecoder)
        (Decode.field "tiPred" Decode.string)
        (Decode.field "tiFunctionInDocument" (Decode.nullable timexFunctionInDocumentDecoder))
        (Decode.field "tiType" timexTypeDecoder)
        (Decode.field "tiTemporalFunction" (Decode.nullable timexTemporalFunctionDecoder))
        (Decode.field "tiLingValue" Decode.string)
        (Decode.field "tiValue" Decode.string)
        (Decode.field "tiMod" (Decode.nullable timexModDecoder))
        (Decode.field "tiAnchor" (Decode.nullable addrDecoder))
        (Decode.field "tiBeginPoint" (Decode.nullable addrDecoder))
        (Decode.field "tiEndPoint" (Decode.nullable addrDecoder))
        (Decode.field "tiQuant" (Decode.nullable Decode.string))
        (Decode.field "tiFreq" (Decode.nullable Decode.string))


timexCalendarDecoder : Decode.Decoder TimexCalendar
timexCalendarDecoder = attrDecoder timexCalendarStr


timexFunctionInDocumentDecoder : Decode.Decoder TimexFunctionInDocument
timexFunctionInDocumentDecoder = attrDecoder timexFunctionInDocumentStr


timexTypeDecoder : Decode.Decoder TimexType
timexTypeDecoder = attrDecoder timexTypeStr


timexTemporalFunctionDecoder : Decode.Decoder TimexTemporalFunction
timexTemporalFunctionDecoder = attrDecoder timexTemporalFunctionStr


timexModDecoder : Decode.Decoder TimexMod
timexModDecoder = attrDecoder timexModStr


addrDecoder : Decode.Decoder Addr
addrDecoder =
    andThen (Decode.index 0 Decode.int) (\x ->
    andThen (Decode.index 1 Decode.int) (\y ->
    Decode.succeed (x, y)
    ))


---------------------------------------------------
-- JSON Decoding Utils
---------------------------------------------------


-- | Since map9 (and higher) are not in Json.Decode...
decodeMap9
    : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> Decode.Decoder a
    -> Decode.Decoder b
    -> Decode.Decoder c
    -> Decode.Decoder d
    -> Decode.Decoder e
    -> Decode.Decoder f
    -> Decode.Decoder g
    -> Decode.Decoder h
    -> Decode.Decoder i
    -> Decode.Decoder value
decodeMap9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 =
    andThen m1 (\x1 ->
    andThen m2 (\x2 ->
    andThen m3 (\x3 ->
    andThen m4 (\x4 ->
    andThen m5 (\x5 ->
    andThen m6 (\x6 ->
    andThen m7 (\x7 ->
    andThen m8 (\x8 ->
    andThen m9 (\x9 -> Decode.succeed (f x1 x2 x3 x4 x5 x6 x7 x8 x9)
    )))))))))


-- | Since map9 (and higher) are not in Json.Decode...
decodeMap11
    : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> value)
    -> Decode.Decoder a
    -> Decode.Decoder b
    -> Decode.Decoder c
    -> Decode.Decoder d
    -> Decode.Decoder e
    -> Decode.Decoder f
    -> Decode.Decoder g
    -> Decode.Decoder h
    -> Decode.Decoder i
    -> Decode.Decoder j
    -> Decode.Decoder k
    -> Decode.Decoder value
decodeMap11 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 =
    andThen m1 (\x1 ->
    andThen m2 (\x2 ->
    andThen m3 (\x3 ->
    andThen m4 (\x4 ->
    andThen m5 (\x5 ->
    andThen m6 (\x6 ->
    andThen m7 (\x7 ->
    andThen m8 (\x8 ->
    andThen m9 (\x9 ->
    andThen m10 (\x10 ->
    andThen m11 (\x11 -> Decode.succeed (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
    )))))))))))


-- | Since map9 (and higher) are not in Json.Decode...
decodeMap12
    : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> value)
    -> Decode.Decoder a
    -> Decode.Decoder b
    -> Decode.Decoder c
    -> Decode.Decoder d
    -> Decode.Decoder e
    -> Decode.Decoder f
    -> Decode.Decoder g
    -> Decode.Decoder h
    -> Decode.Decoder i
    -> Decode.Decoder j
    -> Decode.Decoder k
    -> Decode.Decoder l
    -> Decode.Decoder value
-- decodeMap12 = Debug.crash "asdf"
decodeMap12 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 =
    andThen m1 (\x1 ->
    andThen m2 (\x2 ->
    andThen m3 (\x3 ->
    andThen m4 (\x4 ->
    andThen m5 (\x5 ->
    andThen m6 (\x6 ->
    andThen m7 (\x7 ->
    andThen m8 (\x8 ->
    andThen m9 (\x9 ->
    andThen m10 (\x10 ->
    andThen m11 (\x11 ->
    andThen m12 (\x12 -> Decode.succeed (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
    ))))))))))))


-- | Since map9 (and higher) are not in Json.Decode...
decodeMap13
    : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> value)
    -> Decode.Decoder a
    -> Decode.Decoder b
    -> Decode.Decoder c
    -> Decode.Decoder d
    -> Decode.Decoder e
    -> Decode.Decoder f
    -> Decode.Decoder g
    -> Decode.Decoder h
    -> Decode.Decoder i
    -> Decode.Decoder j
    -> Decode.Decoder k
    -> Decode.Decoder l
    -> Decode.Decoder m
    -> Decode.Decoder value
decodeMap13 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 =
    andThen m1 (\x1 ->
    andThen m2 (\x2 ->
    andThen m3 (\x3 ->
    andThen m4 (\x4 ->
    andThen m5 (\x5 ->
    andThen m6 (\x6 ->
    andThen m7 (\x7 ->
    andThen m8 (\x8 ->
    andThen m9 (\x9 ->
    andThen m10 (\x10 ->
    andThen m11 (\x11 ->
    andThen m12 (\x12 ->
    andThen m13 (\x13 -> Decode.succeed (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)
    )))))))))))))


-- | Just a version of Decode.andThen with swapped arguments.
andThen : Decode.Decoder a -> (a -> Decode.Decoder b) -> Decode.Decoder b
andThen f m = Decode.andThen m f


-- -- | Since map9 is not in Json.Decode...
-- decodeMap3
--     : (a -> b -> c -> value)
--     -> Decode.Decoder a
--     -> Decode.Decoder b
--     -> Decode.Decoder c
--     -> Decode.Decoder value
-- decodeMap3 f m1 m2 m3 =
--     andThen m1 (\x1 ->
--     andThen m2 (\x2 ->
--     andThen m3 (\x3 -> Decode.succeed (f x1 x2 x3)
--     )))
