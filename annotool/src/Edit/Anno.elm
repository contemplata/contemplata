-- | Fundamental annotation-related types.


module Edit.Anno exposing
  ( Event(..)
  , EventClass(..)
  , EventTense(..)
  , eventDefault

  -- * JSON
  , encodeEvent
  , eventDecoder
  )


import Json.Decode as Decode
import Json.Encode as Encode

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


-- | An event's tense. The default is `None`, which should be represented by
-- `Nothing`.
type EventTense
    = Future
    | Past
    | Present
    | Imperfect


eventDefault : Event
eventDefault = Event
  { evClass = Aspectual
  , evTense = Just Future }


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
