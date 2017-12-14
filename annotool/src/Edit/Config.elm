-- | Annotation-related types.


module Edit.Config exposing
  ( Entity
  , EntityType
  , Attr (..)
  )


import Dict as D

---------------------------------------------------
-- Configuration
---------------------------------------------------


-- | A configuration to which an `Entity` must correspond.
type alias Entity =
  { name : String
  , typ : EntityType
  , attributes : D.Dict String Attr
  }

-- | Entity type configuration.
type alias EntityType =
  { among : List String
  , def : Maybe String
  }


-- | Attribute configuration.
type Attr
  = Closed
    { among : List String
    , def : Maybe String
    , required : Bool }
  | Free
    { def : Maybe String }
  | Anchor

