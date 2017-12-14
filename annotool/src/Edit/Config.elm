-- | Annotation-related types.


module Edit.Config exposing
  ( Config
  , Entity
  , EntityType
  , Attr (..)
  , configDecoder
  )


import Json.Decode as Decode
import Json.Encode as Encode

import Dict as D


---------------------------------------------------
-- Configuration
---------------------------------------------------


-- | A configuration to which an `Entity` must correspond.
type alias Config =
  { entities : List Entity
  }


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


---------------------------------------------------
-- JSON
---------------------------------------------------


configDecoder : Decode.Decoder Config
configDecoder =
  let mkConfig ents = {entities = ents}
  in  Decode.map mkConfig
        (Decode.field "entities" (Decode.list entityDecoder))


entityDecoder : Decode.Decoder Entity
entityDecoder =
  let mkEntity name typ atts =
        { name = name
        , typ = typ
        , attributes = atts
        }
  in  Decode.map3 mkEntity
        (Decode.field "name" Decode.string)
        (Decode.field "typ" typDecoder)
        (Decode.field "attributes" attrMapDecoder)


typDecoder : Decode.Decoder EntityType
typDecoder =
  let mkTyp among def =
        { among = among
        , def = def }
  in  Decode.map2 mkTyp
        (Decode.field "among" (Decode.list Decode.string))
        (Decode.field "def" (Decode.nullable Decode.string))


attrMapDecoder : Decode.Decoder (D.Dict String Attr)
attrMapDecoder = Decode.dict attrDecoder


attrDecoder : Decode.Decoder Attr
attrDecoder =
    Decode.oneOf
        [ closedDecoder
        , freeDecoder
        , anchorDecoder ]


closedDecoder : Decode.Decoder Attr
closedDecoder =
    Decode.map4 (\_ among def required -> Closed {among=among, def=def, required=required})
      (Decode.field "tag" (isString "Closed"))
      (Decode.field "among" (Decode.list Decode.string))
      (Decode.field "def" (Decode.nullable Decode.string))
      (Decode.field "required" Decode.bool)


freeDecoder : Decode.Decoder Attr
freeDecoder =
    Decode.map2 (\_ def -> Free {def = def})
      (Decode.field "tag" (isString "Free"))
      (Decode.field "def" (Decode.nullable Decode.string))


anchorDecoder : Decode.Decoder Attr
anchorDecoder =
    Decode.map (\_ -> Anchor)
        (Decode.field "tag" (isString "Anchor"))


---------------------------------------------------
-- JSON Utils
---------------------------------------------------


isString : String -> Decode.Decoder ()
isString str0
    =  Decode.string
    |> Decode.andThen
       (\str ->
            if str == str0
            then Decode.succeed ()
            else Decode.fail <| "The two strings differ: " ++ str0 ++ " /= " ++ str
       )
