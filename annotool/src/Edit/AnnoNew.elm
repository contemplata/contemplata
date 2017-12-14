-- | Annotation-related types.


module Edit.AnnoNew exposing
  (
  -- * Nodes (in general)
    nodeLabelSet
  , phrasalLabelSet
  , preTerminalLabelSet
  , NodeAttr (..)
  )


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
  { name = cfg.name
  , typ =
      case cfg.typ.def of
          Just val -> val
          Nothing ->
              case cfg.typ.among of
                  val :: _ -> val
                  [] -> Debug.crash "Anno.defaultEntity: empty list of types"
  , attributes =
      let onPair (name, attrCfg) =
              case defaultAttr attrCfg of
                  Nothing -> Nothing
                  Just attr -> Just (name, attr)
      in  D.fromList <| List.filterMap onPair <| D.toList cfg.attributes
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
