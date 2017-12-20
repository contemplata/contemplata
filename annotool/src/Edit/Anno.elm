-- | Annotation-related types.


module Edit.Anno exposing
  (
  -- * Default
    defaultEntity
  , defaultAttr
  )


import Json.Decode as Decode
import Json.Encode as Encode

-- import Focus exposing ((=>))
import Focus as Lens

import List as L
import Dict as D

import Util
import Edit.Config as Cfg
import Edit.Core exposing (Addr)
import Edit.Anno.Core exposing (..)


---------------------------------------------------
-- Default annotations
---------------------------------------------------


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
        Cfg.Anchor -> Nothing
