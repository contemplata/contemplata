-- | Fundamental annotation-related types.


module Edit.Anno exposing
  ( Event(..)
  , EventClass(..)
  , EventTense(..)
  , eventDefault
  , eventClassStr
  , eventClassFromStr
  , eventTenseStr
  , eventTenseFromStr

  -- * JSON
  , encodeEvent
  , eventDecoder
  )


import Json.Decode as Decode
import Json.Encode as Encode

import Dict as D

import Util


-- | Information specific to an event.
type Event = Event
  { evClass : EventClass
  , evTense : Maybe EventTense
  }


-- | A class of an event.
type EventClass
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


-- | The string representations of an event class.
eventClassStr : List (String, EventClass)
eventClassStr =
  [ ("Aspectual", Aspectual)
  , ("Cause", Cause)
  , ("EventContainer", EventContainer)
  , ("IAction", IAction)
  , ("IState", IState)
  , ("Modal", Modal)
  , ("Occurrence", Occurrence)
  , ("Perception", Perception)
  , ("Reporting", Reporting)
  , ("State", State) ]


eventClassFromStr : String -> EventClass
eventClassFromStr x =
  let
      d = D.fromList eventClassStr
  in
      case D.get x d of
          Nothing -> Debug.crash "Anno.eventClassFromStr: cannot decode"
          Just y -> y


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
type EventTense
    = Future
    | Past
    | Present
    | Imperfect


-- | The string representations of an event class.
eventTenseStr : List (String, Maybe EventTense)
eventTenseStr =
  [ ("Future", Just Future)
  , ("Past", Just Past)
  , ("Present", Just Present)
  , ("Imperfect", Just Imperfect)
  , ("--", Nothing) ]


eventTenseFromStr : String -> Maybe EventTense
eventTenseFromStr x =
  case D.get x (D.fromList eventTenseStr) of
    Nothing -> Nothing
    Just y -> y


eventDefault : Event
eventDefault = Event
  { evClass = Aspectual
  , evTense = Nothing }


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


eventDecoder : Decode.Decoder Event
eventDecoder =
  Decode.map2 (\cls tns -> Event {evClass=cls, evTense=tns})
    (Decode.field "evClass" eventClassDecoder)
    (Decode.field "evTense" (Decode.nullable eventTenseDecoder))


eventClassDecoder : Decode.Decoder EventClass
eventClassDecoder =
  let
    decode x = case x of
        "Aspectual" -> Aspectual
        "Cause" -> Cause
        "EventContainer" -> EventContainer
        "IAction" -> IAction
        "IState" -> IState
        "Modal" -> Modal
        "Occurrence" -> Occurrence
        "Perception" -> Perception
        "Reporting" -> Reporting
        "State" -> State
        _ -> Debug.crash ("Anno.eventClassDecoder -- unknown value: " ++ x)
  in
    Decode.map decode Decode.string


eventTenseDecoder : Decode.Decoder EventTense
eventTenseDecoder =
  let
    decode x = case x of
      "Future" -> Future
      "Past" -> Past
      "Present" -> Present
      "Imperfect" -> Imperfect
      _ -> Debug.crash ("Anno.eventTenseDecoder -- unknown value: " ++ x)
  in
    Decode.map decode Decode.string


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeEvent : Event -> Encode.Value
encodeEvent (Event r) = Encode.object
    [ ("evClass", encodeEventClass r.evClass)
    , ("evTense", Util.encodeMaybe encodeEventTense r.evTense)
    ]


encodeEventClass : EventClass -> Encode.Value
encodeEventClass cls = Encode.string <|
    case cls of
        Aspectual -> "Aspectual"
        Cause -> "Cause"
        EventContainer -> "EventContainer"
        IAction -> "IAction"
        IState -> "IState"
        Modal -> "Modal"
        Occurrence -> "Occurrence"
        Perception -> "Perception"
        Reporting -> "Reporting"
        State -> "State"


encodeEventTense : EventTense -> Encode.Value
encodeEventTense tns = Encode.string <|
    case tns of
        Future -> "Future"
        Past -> "Past"
        Present -> "Present"
        Imperfect -> "Imperfect"
