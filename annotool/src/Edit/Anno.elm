-- | Fundamental annotation-related types.


module Edit.Anno exposing
  ( Event(..)
  , EventAttr (..)
  , EventType(..)
  , EventClass(..)
  , EventTense(..)
  , EventAspect(..)
  , EventPolarity(..)
  , EventModality(..)
  , eventDefault
  , eventClassStr
  -- , eventClassFromStr
  , eventTypeStr
  , eventTenseStr
  -- , eventTenseFromStr
  , eventAspectStr
  , eventPolarityStr
  , eventSubjMoodStr
  , eventModalityStr

  -- * JSON
  , encodeEvent
  , eventDecoder

  -- * Utils
  , nullable
  , valueFromStr
  )


import Json.Decode as Decode
import Json.Encode as Encode

import Dict as D

import Util


---------------------------------------------------
-- Event
---------------------------------------------------


-- | Information specific to an event.
type Event = Event
  { evClass : EventClass
  , evType : EventType
  , evTense : Maybe EventTense
  , evAspect : Maybe EventAspect
  , evPolarity : EventPolarity
  , evSubjMood : Bool
  , evModality : Maybe EventModality
  , evComment : String
  -- , evConfidence :: Confidence
  }


eventDefault : Event
eventDefault = Event
  { evClass = Aspectual
  , evType = Transition
  , evTense = Nothing
  , evAspect = Nothing
  , evPolarity = Pos
  , evSubjMood = False
  , evModality = Nothing
  , evComment = ""
  }


type EventAttr
    = ClassAttr EventClass
    | TypeAttr EventType
    | TenseAttr (Maybe EventTense)
    | AspectAttr (Maybe EventAspect)
    | PolarityAttr EventPolarity
    | SubjMoodAttr Bool
    | ModalityAttr (Maybe EventModality)
    | CommentAttr String


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
--     | Cause
--     | EventContainer
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
-- Tense
---------------------------------------------------


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
type EventTense
    = Future
    | Past
    | Present
    | Imperfect


-- | The string representations of an event class.
eventTenseStr : List (String, EventTense)
eventTenseStr =
  [ ("Future", Future)
  , ("Past", Past)
  , ("Present", Present)
  , ("Imperfect", Imperfect) ]


eventTenseFromStr : String -> Maybe EventTense
eventTenseFromStr = valueFromStrSafe eventTenseStr
--   case D.get x (D.fromList eventTenseStr) of
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
    | Imperfective
    | PerfectiveProgressive
    | ImperfectiveProgressive


-- | The string representations of an event class.
eventAspectStr : List (String, EventAspect)
eventAspectStr =
  [ ("Progressive", Progressive)
  , ("Perfective", Perfective)
  , ("Imperfective", Imperfective)
  , ("PerfectiveProgressive", PerfectiveProgressive)
  , ("ImperfectiveProgressive", ImperfectiveProgressive) ]


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


-- | The string representations of an event class.
eventSubjMoodStr : List (String, Bool)
eventSubjMoodStr =
  [ ("True", True)
  , ("False", False) ]


---------------------------------------------------
-- Modality
---------------------------------------------------


-- | An event's modality.
type EventModality
    = Modality1
    | Modality2


-- | The string representations of an event class.
eventModalityStr : List (String, EventModality)
eventModalityStr =
  [ ("Modality1", Modality1)
  , ("Modality2", Modality2) ]


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
  let mkEvent cls typ tns asp pol subj mod com =
        Event
        { evClass=cls
        , evType=typ
        , evTense=tns
        , evAspect=asp
        , evPolarity=pol
        , evSubjMood=subj
        , evModality=mod
        , evComment=com
        }
  in  Decode.map8 mkEvent
        (Decode.field "evClass" eventClassDecoder)
        (Decode.field "evType" eventTypeDecoder)
        (Decode.field "evTense" (Decode.nullable eventTenseDecoder))
        (Decode.field "evAspect" (Decode.nullable eventAspectDecoder))
        (Decode.field "evPolarity" eventPolarityDecoder)
        (Decode.field "evSubjMood" Decode.bool)
        (Decode.field "evModality" (Decode.nullable eventModalityDecoder))
        (Decode.field "evComment" Decode.string)


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


eventTenseDecoder : Decode.Decoder EventTense
eventTenseDecoder =
  let
    decode = valueFromStr eventTenseStr
--     decode x = case x of
--       "Future" -> Future
--       "Past" -> Past
--       "Present" -> Present
--       "Imperfect" -> Imperfect
--       _ -> Debug.crash ("Anno.eventTenseDecoder -- unknown value: " ++ x)
  in
    Decode.map decode Decode.string


eventAttrDecoder : List (String, a) -> Decode.Decoder a
eventAttrDecoder lst =
  let decode = valueFromStr lst
  in  Decode.map decode Decode.string


eventTypeDecoder : Decode.Decoder EventType
eventTypeDecoder = eventAttrDecoder eventTypeStr


eventAspectDecoder : Decode.Decoder EventAspect
eventAspectDecoder = eventAttrDecoder eventAspectStr


eventPolarityDecoder : Decode.Decoder EventPolarity
eventPolarityDecoder = eventAttrDecoder eventPolarityStr


eventModalityDecoder : Decode.Decoder EventModality
eventModalityDecoder = eventAttrDecoder eventModalityStr


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeEvent : Event -> Encode.Value
encodeEvent (Event r) = Encode.object
    [ ("evClass", encodeEventClass r.evClass)
    , ("evType", encodeEventType r.evType)
    , ("evTense", Util.encodeMaybe encodeEventTense r.evTense)
    , ("evAspect", Util.encodeMaybe encodeEventAspect r.evAspect)
    , ("evPolarity", encodeEventPolarity r.evPolarity)
    , ("evSubjMood", Encode.bool r.evSubjMood)
    , ("evModality", Util.encodeMaybe encodeEventModality r.evModality)
    , ("evComment", Encode.string r.evComment)
    ]


encodeEventClass : EventClass -> Encode.Value
encodeEventClass cls = Encode.string <|
  strFromValue eventClassStr cls

encodeEventType : EventType -> Encode.Value
encodeEventType x = Encode.string <|
  strFromValue eventTypeStr x


encodeEventTense : EventTense -> Encode.Value
encodeEventTense x = Encode.string <|
  strFromValue eventTenseStr x


encodeEventAspect : EventAspect -> Encode.Value
encodeEventAspect x = Encode.string <|
  strFromValue eventAspectStr x


encodeEventPolarity : EventPolarity -> Encode.Value
encodeEventPolarity x = Encode.string <|
  strFromValue eventPolarityStr x


encodeEventModality : EventModality -> Encode.Value
encodeEventModality x = Encode.string <|
  strFromValue eventModalityStr x
