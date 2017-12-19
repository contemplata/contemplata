-- | Annotation-related types.


module Edit.Config exposing
  ( Config
  , Entity
  , EntityType
  , Attr (..)
  , ClosedRec
  , FreeRec
  , configDecoder
  , entityConfig
  -- , attrConfig
  )


import Json.Decode as Decode
import Json.Encode as Encode

import Util
import Dict as D


---------------------------------------------------
-- Configuration
---------------------------------------------------


-- | A configuration to which an `Entity` must correspond.
type alias Config =
  { entities : List Entity
    -- ^ TODO: This could be a map!
  , nonTerminals : List String
  , preTerminals : List String
  }


-- | A configuration to which an `Entity` must correspond.
type alias Entity =
  { name : String
  , typ : EntityType
  -- , attributes : D.Dict String Attr
  , attributes : List (String, Attr)
  , attributesOnType : D.Dict String (List (String, Attr))
  }


-- | Entity type configuration.
type alias EntityType =
  { among : List String
  , def : Maybe String
  }


type alias ClosedRec =
  { among : List String
  , def : Maybe String
  , required : Bool }


type alias FreeRec =
  { def : Maybe String }


-- | Attribute configuration.
type Attr
  = Closed ClosedRec
  | Free FreeRec
  | Anchor


-- -- -- | Retrieve the list of the values for the given attribute.
-- -- attrConfig
-- --     :  String -- ^ Entity name
-- --     -> String -- ^ Attribute name
-- --     -> Config -- ^ Config, obviously
-- --     -> Attr
-- -- attrConfig name attr cfg =
-- --     let err = "Config.attrConfig: invalid attribute name" in
-- --     case Util.find (\x -> x.name == name) cfg.entities of
-- --         Nothing -> Debug.crash err
-- --         Just ent ->
-- --             case D.get attr ent.attributes of
-- --                 Nothing -> Debug.crash err
-- --                 Just at -> at
--
--
-- -- | Retrieve the list of values for the given attribute.
-- attrConfig
--     :  String -- ^ Attribute name
--     -> Entity -- ^ Entity config
--     -> Attr
-- attrConfig attr ent =
--     let err = "Config.attrConfig: invalid attribute name" in
--     -- case D.get attr ent.attributes of
--     case Util.find (\(x, _) -> attr == x) ent.attributes of
--         Nothing -> Debug.crash err
--         Just (_, val) -> val


-- | Get the configuration for a given entity name.
entityConfig
    :  String -- ^ Entity name
    -> Config -- ^ Config
    -> Entity
entityConfig name cfg =
    case Util.find (\x -> x.name == name) cfg.entities of
        Nothing -> Debug.crash "Config.entityConfig: unknown entity name"
        Just en -> en


---------------------------------------------------
-- JSON
---------------------------------------------------


configDecoder : Decode.Decoder Config
configDecoder =
  let mkConfig ents nons pres =
        { entities = ents
        , nonTerminals = nons
        , preTerminals = pres
        }
  in  Decode.map3 mkConfig
        (Decode.field "entities" (Decode.list entityDecoder))
        (Decode.field "nonTerminals" (Decode.list Decode.string))
        (Decode.field "preTerminals" (Decode.list Decode.string))


entityDecoder : Decode.Decoder Entity
entityDecoder =
  let mkEntity name typ atts attsOnType =
        { name = name
        , typ = typ
        , attributes = atts
        , attributesOnType = attsOnType
        }
  in  Decode.map4 mkEntity
        (Decode.field "name" Decode.string)
        (Decode.field "typ" typDecoder)
        (Decode.field "attributes" attrListDecoder)
        (Decode.field "attributesOnType" attsOnTypeDecoder)


typDecoder : Decode.Decoder EntityType
typDecoder =
  let mkTyp among def =
        { among = among
        , def = def }
  in  Decode.map2 mkTyp
        (Decode.field "among" (Decode.list Decode.string))
        (Decode.field "def" (Decode.nullable Decode.string))


-- attrMapDecoder : Decode.Decoder (D.Dict String Attr)
-- attrMapDecoder = Decode.dict attrDecoder


attrListDecoder : Decode.Decoder (List (String, Attr))
attrListDecoder =
    let
      pairDecoder = Decode.map2 (\name val -> (name, val))
        (Decode.index 0 Decode.string)
        (Decode.index 1 attrDecoder)
    in
      Decode.list pairDecoder


attsOnTypeDecoder : Decode.Decoder (D.Dict String (List (String, Attr)))
attsOnTypeDecoder = Decode.dict attrListDecoder


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
