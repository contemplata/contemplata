-- | Fundamental annotation-related types.


module Edit.Anno exposing
  ( Event(..)
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
  , evInquisit : EventInquisit
  , evTime : Maybe EventTime
  , evAspect : Maybe EventAspect
  , evPolarity : EventPolarity
  , evMood : Maybe EventMood
  , evModality : Maybe EventModality
  , evCardinality : String
  , evMod : Maybe EventMod
  , evComment : String
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
  , evComment = ""
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
  let mkEvent cls typ inq tns asp pol subj modal car mod com =
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
        , evComment=com
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


eventAttrDecoder : List (String, a) -> Decode.Decoder a
eventAttrDecoder lst =
  let decode = valueFromStr lst
  in  Decode.map decode Decode.string


eventTypeDecoder : Decode.Decoder EventType
eventTypeDecoder = eventAttrDecoder eventTypeStr


eventInquisitDecoder : Decode.Decoder EventInquisit
eventInquisitDecoder = eventAttrDecoder eventInquisitStr


eventAspectDecoder : Decode.Decoder EventAspect
eventAspectDecoder = eventAttrDecoder eventAspectStr


eventPolarityDecoder : Decode.Decoder EventPolarity
eventPolarityDecoder = eventAttrDecoder eventPolarityStr


eventMoodDecoder : Decode.Decoder EventMood
eventMoodDecoder = eventAttrDecoder eventMoodStr


eventModalityDecoder : Decode.Decoder EventModality
eventModalityDecoder = eventAttrDecoder eventModalityStr


eventModDecoder : Decode.Decoder EventMod
eventModDecoder = eventAttrDecoder eventModStr


-- | Since map9 is not in Json.Decode...
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
-- decodeMap11 = Debug.crash "asdf"
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
    , ("evComment", Encode.string r.evComment)
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
