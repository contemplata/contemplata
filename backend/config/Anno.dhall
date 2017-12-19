{ 
-- Allowed types of node annotations
  entities =
    [ ./Anno/timex.val
    , ./Anno/event.val
    , ./Anno/signal.val
    ] : List ./Anno/Entity.typ

-- Phrasal and part-of-speech tags
, nonTerminals = ./Anno/nonTerminals.val
, preTerminals = ./Anno/preTerminals.val

-- Annotation levels
, annoLevels = ["orig", "syntax", "temporal", "relations"]
}
