-- | Annotation-related types.


module Edit.Anno exposing
  (
  -- * Nodes (in general)
    nodeLabelSet
  , phrasalLabelSet
  , preTerminalLabelSet
  , NodeAttr (..)

  -- * Annotations
  , Entity
  , Attr (..)

  -- * Default
  , defaultEntity
  , defaultAttr

  -- * JSON
  , encodeEntity
  , entityDecoder

  -- * Lenses
  , entityType
  , entityAttr
  )


import Json.Decode as Decode
import Json.Encode as Encode

-- import Focus exposing ((=>))
import Focus as Lens

import List as L
import Dict as D

import Util
import Edit.Config as Cfg


---------------------------------------------------
-- General
---------------------------------------------------


nodeLabelSet : List String
nodeLabelSet = phrasalLabelSet ++ preTerminalLabelSet


phrasalLabelSet : List String
phrasalLabelSet =
    let
        baseList =
            [ "AP"
            , "AdP"
            , "COORD"
            , "MWA"
            , "MWADV"
            , "MWC"
            , "MWCL"
            , "MWD"
            , "MWN"
            , "MWP"
            , "MWPRO"
            , "MWV"
            , "NP"
            , "PP"
            , "ROOT"
            , "SENT"
            , "Sint"
            , "Srel"
            , "Ssub"
            , "VN"
            , "VP"
            , "VPinf"
            , "VPpart"
            -- below, custom additional labels
            , "PARA" ]
        addDolar x = "$" ++ x
    in
        baseList ++ List.map addDolar baseList


preTerminalLabelSet : List String
preTerminalLabelSet =
  [ "ADJ"
  , "ADJWH"
  , "ADV"
  , "ADVWH"
  , "C"
  , "CC"
  , "CS"
  , "CL"
  , "CLO"
  , "CLR"
  , "CLS"
  , "DET"
  , "DETWH"
  , "ET"
  , "I"
  , "N"
  , "NC"
  , "NPP"
  , "P"
  , "PREF"
  , "PRO"
  , "PROREL"
  , "PROWH"
  , "PUNC"
  , "V"
  , "VIMP"
  , "VINF"
  , "VPP"
  , "VPR"
  , "VS" ]
  -- below, custom additional labels
  -- , "PARA"
  -- , "$" ]


-- | Changing a node attribute.
type NodeAttr
    = NodeLabelAttr String
    | NodeCommentAttr String


---------------------------------------------------
-- Annotations
---------------------------------------------------


-- | A generic annotation entity (e.g. Event, Timex, ...).
type alias Entity =
  { name : String
  , typ : String
  , attributes : D.Dict String Attr
    -- ^ The value of optional attributes does not have to be specified in the
    -- map above.
  }


-- | Corresponding to `Odil.Config.Attr`.
type Attr
  = Attr String -- ^ A closed or free attribute.
  | Anchor


defaultEntity : Cfg.Entity -> Entity
defaultEntity cfg =
  let
    typ0 =
        case cfg.typ.def of
            Just val -> val
            Nothing ->
                case cfg.typ.among of
                    val :: _ -> val
                    [] -> Debug.crash "Anno.defaultEntity: empty list of types"
  in
    { name = cfg.name
    , typ = typ0
    , attributes =
        let onPair (name, attrCfg) =
                case defaultAttr attrCfg of
                    Nothing -> Nothing
                    Just attr -> Just (name, attr)
        in  D.fromList <|
            List.filterMap onPair <|
            -- D.toList cfg.attributes
            cfg.attributes ++
                (Maybe.withDefault [] <| D.get typ0 cfg.attributesOnType)
    }


defaultAttr : Cfg.Attr -> Maybe Attr
defaultAttr cfg =
    case cfg of
        Cfg.Closed r ->
            let def0 =
                    if r.required
                    then List.head r.among
                    else Nothing
            in  Maybe.map Attr <| Util.mappend r.def def0
        Cfg.Free r -> Maybe.map Attr r.def
        Cfg.Anchor -> Just Anchor


----------------------------
-- Lenses
----------------------------


-- | A lens for the type of the entity.
entityType : Lens.Focus Entity String
entityType =
  let
    get ent = ent.typ
    update f ent = {ent | typ = f ent.typ}
  in
    Lens.create get update


-- | A lens for the given attribute.
entityAttr
    : String
      -- ^ The name of the attribute
    -> Lens.Focus Entity (Maybe Attr)
entityAttr attrName =
  let
    get ent =
        D.get attrName ent.attributes
    update f ent =
        {ent | attributes = D.update attrName f ent.attributes}
  in
    Lens.create get update


---------------------------------------------------
-- JSON: Decoding
---------------------------------------------------


entityDecoder : Decode.Decoder Entity
entityDecoder =
  let mkEntity name typ atts =
        { name = name
        , typ = typ
        , attributes = atts
        }
  in  Decode.map3 mkEntity
        (Decode.field "name" Decode.string)
        (Decode.field "typ" Decode.string)
        (Decode.field "attributes" attrMapDecoder)


attrMapDecoder : Decode.Decoder (D.Dict String Attr)
attrMapDecoder = Decode.dict attrDecoder


attrDecoder : Decode.Decoder Attr
attrDecoder = Decode.oneOf [pureAttrDecoder, anchorDecoder]


pureAttrDecoder : Decode.Decoder Attr
pureAttrDecoder =
  Decode.map2 (\_ val -> Attr val)
    (Decode.field "tag" (isString "Attr"))
    (Decode.field "contents" Decode.string)


anchorDecoder : Decode.Decoder Attr
anchorDecoder =
  Decode.map (\_ -> Anchor)
    (Decode.field "tag" (isString "Anchor"))


isString : String -> Decode.Decoder ()
isString str0
    =  Decode.string
    |> Decode.andThen
       (\str ->
            if str == str0
            then Decode.succeed ()
            else Decode.fail <| "The two strings differ: " ++ str0 ++ " /= " ++ str
       )


---------------------------------------------------
-- JSON: Encoding
---------------------------------------------------


encodeEntity : Entity -> Encode.Value
encodeEntity r =
  Encode.object
    [ ("tag", Encode.string "Entity")
    , ("name", Encode.string r.name)
    , ("typ", Encode.string r.typ)
    , ("attributes", encodeAttrMap r.attributes)
    ]


encodeAttrMap : D.Dict String Attr -> Encode.Value
encodeAttrMap =
  let encodePair (key, val) = (key, encodeAttr val)
  in  Encode.object << L.map encodePair << D.toList


encodeAttr : Attr -> Encode.Value
encodeAttr attr =
  case attr of
    Attr x -> Encode.object
      [ ("tag", Encode.string "Attr")
      , ("contents", Encode.string x)
      ]
    Anchor -> Encode.object
      [ ("tag", Encode.string "Anchor")
      ]


---------------------------------------------------
-- Annotation modifications
---------------------------------------------------


-- | To signal a change of the attribute type.
type alias EntityType =
    { name : String
    , typ : String
    }


-- | To signal a change of the attribute value.
type alias EntityAttr =
    { name : String
    , attr : Maybe Attr
      -- ^ `Nothing` when the value should be deleted from the attributes map.
      -- TODO: should we make sure we don't delete a required attribute?
      -- In general, we should somehow make sure that the change is consistent
      -- with the config.
    }


---------------------------------------------------
-- Utils
---------------------------------------------------


-- toInt : String -> Int
-- toInt x = String.toInt x |> Result.toMaybe |> Maybe.withDefault 0
--
--
-- mapKeys
--     : (comparable -> comparable2)
--     -> D.Dict comparable c
--     -> D.Dict comparable2 c
-- mapKeys f d =
--   let first f (x, y) = (f x, y)
--   in  D.fromList <| L.map (first f) <| D.toList <| d
