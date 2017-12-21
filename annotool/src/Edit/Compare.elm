module Edit.Compare exposing
  ( compareSyntax
  )


import Basics exposing (Order(..))
import Set as S
import Dict as D
import Compare as Cmp
import Focus as Lens

import Rose as R

import Edit.Core exposing (..)
import Edit.Anno.Core as Anno
import Edit.Anno as Anno
import Edit.Model as M


---------------------------------------------------
-- Top-level
---------------------------------------------------


-- | Comparison context
type alias Context = PartId -> R.Tree M.Node


-- | Compare the basic, syntactic layer of two trees.
--
-- The resulting set of node IDs for a particular input tree will contain all
-- the nodes which are not present in the other input tree.
--
-- We say that two nodes are different if any of the following holds:
-- * Their labels are different
-- * Their spans are different
-- * The lists of the spans of their children are different
--
-- The third one is supposed to account for the potential tree non-projectivity.
compareSyntax
    :  (Context, PartId)
    -- ^ The first context with the selected tree
    -> (Context, PartId)
    -- ^ The other context with the selected tree
    -> (List NodeId, List NodeId)
compareSyntax (ctx, ix) (cty, iy) =
    let tx = ctx ix
        ty = cty iy
        infos ct = List.sortWith compNodeInfo << R.flatten << extractNodeInfo ct
    in  diffNodeInfo (infos ctx tx) (infos cty ty)


-- | Compare two, ordered lists of infos.  The resulting pair (xs, ys) contains
-- the missing node IDs in the two respective input lists.
diffNodeInfo : List NodeInfo -> List NodeInfo -> (List NodeId, List NodeId)
diffNodeInfo xs ys =
    case (xs, ys) of
        (xHead :: xTail, yHead :: yTail) ->
            case compNodeInfo xHead yHead of
                EQ -> diffNodeInfo xTail yTail
                LT -> consFst xHead.id <| diffNodeInfo xTail ys
                GT -> consSnd yHead.id <| diffNodeInfo xs yTail
        (xHead :: xTail, []) ->
            consFst xHead.id <| diffNodeInfo xTail []
        ([], yHead :: yTail) ->
            consSnd yHead.id <| diffNodeInfo [] yTail
        ([], []) -> ([], [])


-- | Information about a node important for comparison.
type alias NodeInfo =
    { id : NodeId
    -- ^ ID of the node
    , label : String
    -- ^ The label of the node
    , comment : String
    -- ^ The comment of the node
    , annoTyp : Maybe EntityInfo
    -- , annoTyp : Maybe Anno.Entity
    -- ^ Node annotation
    , spans : List (S.Set Int)
    -- ^ The set of positions in the leaves of the subsequent subtrees in the
    -- tree corresponding to the node
    }


-- | Annotation information.
type alias EntityInfo =
  { name : String
  , typ : String
  , attributes : D.Dict String AttrInfo
  }


-- | Relevant information about an attribut.
type AttrInfo
    = AttrVal String
    -- ^ Just a regular string value
    | Anchor AnchorInfo
    -- ^ An anchor


-- | Anchoring information
type alias AnchorInfo =
    { label : String
    -- ^ The label of the target node
    , span : S.Set Int
    -- ^ The span of the target node
    }


-- | Merge the list of span sets.
merge : List (S.Set Int) -> S.Set Int
merge xs =
    case xs of
        head :: tail -> S.union head (merge tail)
        [] -> S.empty


-- | Extract the node informations from the tree.
extractNodeInfo
    : Context
    -- ^ The context to which the following tree belongs
    -> R.Tree M.Node
    -- ^ The tree for which extraction is to be applied
    -> R.Tree NodeInfo
extractNodeInfo ctx (R.Node x ts) =
    case x of
        M.Leaf r -> R.Node
            { id = r.nodeId
            , label = r.nodeVal
            , comment = r.nodeComment
            , annoTyp = Nothing
            , spans = [S.singleton r.leafPos] }
            []
        M.Node r ->
            let
                children = List.map (extractNodeInfo ctx) ts
                value =
                    { id = r.nodeId
                    , comment = r.nodeComment
                    , annoTyp = Maybe.map (extractEntityInfo ctx) r.nodeTyp
                    , label = r.nodeVal
                    , spans = List.map (merge << .spans << R.label) children
                    }
            in
                R.Node value children


-- | Extract attribute information.
extractEntityInfo : Context -> Anno.Entity -> EntityInfo
extractEntityInfo ctx ent =
    { name = ent.name
    , typ = ent.typ
    , attributes = D.map (always <| extractAttrInfo ctx) ent.attributes
    }


-- | Extract attribute information.
extractAttrInfo : Context -> Anno.Attr -> AttrInfo
extractAttrInfo ctx attr =
    case attr of
        Anno.Attr x -> AttrVal x
        Anno.Anchor (treeId, nodeId) ->
            let tree = ctx treeId
            in  Anchor (extractAnchorInfo nodeId tree)


---------------------------------------------------
-- Extracting anchoring information
---------------------------------------------------


-- | Extract anchoring information from the tree.
extractAnchorInfo
    :  NodeId
    -- ^ ID of the anchoring node
    -> R.Tree M.Node
    -- ^ The tree where the anchor resides
    -> AnchorInfo
extractAnchorInfo anchorId tree0 =
    case R.getSubTree (\r -> Lens.get M.nodeId r == anchorId) tree0 of
        Nothing -> {label = "", span = S.empty}
        Just st ->
            { label = Lens.get M.nodeVal (R.label st)
            , span = getSpan st }


-- | Retrieve the span of the given node in the given tree.
getSpan : R.Tree M.Node -> S.Set Int
getSpan =
     let
         span node =
             case node of
                 M.Leaf r -> Just r.leafPos
                 _ -> Nothing
     in
         S.fromList << List.filterMap span << R.flatten


---------------------------------------------------
-- NodeInfo comparison
---------------------------------------------------


-- | Compare two information values.
compNodeInfo : Cmp.Comparator NodeInfo
compNodeInfo =
    Cmp.concat
        [ Cmp.by .label
        , Cmp.by .comment
        , Cmp.compose .annoTyp (maybe compEntityInfo)
        , Cmp.compose .spans compSpanList ]


compSpanList : Cmp.Comparator (List (S.Set Int))
compSpanList = lexico compSpanSet


compSpanSet : Cmp.Comparator (S.Set Int)
compSpanSet =
    Cmp.compose S.toList (lexico Basics.compare)


compEntityInfo : Cmp.Comparator EntityInfo
compEntityInfo =
    Cmp.concat
        [ Cmp.by .name
        , Cmp.by .typ
        , Cmp.compose .attributes compAttrMap ]


compAttrMap : Cmp.Comparator (D.Dict String AttrInfo)
compAttrMap =
    let compPair =
            Cmp.concat
                [ Cmp.by Tuple.first
                , Cmp.compose Tuple.second compAttr ]
    in  Cmp.compose D.toList (lexico compPair)


compAttr : Cmp.Comparator AttrInfo
compAttr attrX attrY =
    case (attrX, attrY) of
        (AttrVal x, AttrVal y) ->
            Basics.compare x y
        (AttrVal _, _) -> LT
        (_, AttrVal _) -> GT
        (Anchor x, Anchor y) ->
            -- Basics.compare x y
            compAnchor x y


compAnchor : Cmp.Comparator AnchorInfo
compAnchor =
    Cmp.concat
        [ Cmp.by .label
        , Cmp.compose .span compSpanSet ]


-- compAnnoTyp : Cmp.Comparator M.NodeAnnoTyp
-- compAnnoTyp annoX annoY =
--     case (annoX, annoY) of
--         (M.NodeEvent evX, M.NodeEvent evY) ->
--             -- Well, yes, we should not probably use `toString` here, but
--             -- really, it's too much trouble to define the comparison function
--             -- for more complex types...
--             Basics.compare (toString evX) (toString evY)
--         (M.NodeEvent _, _) -> LT
--         (_, M.NodeEvent _) -> GT
--         (M.NodeSignal siX, M.NodeSignal siY) ->
--             Basics.compare (toString siX) (toString siY)
--         (M.NodeSignal _, _) -> LT
--         (_, M.NodeSignal _) -> GT
--         (M.NodeTimex tiX, M.NodeTimex tiY) ->
--             Basics.compare (toString tiX) (toString tiY)


---------------------------------------------------
-- Utils
---------------------------------------------------


-- | Lexicographic order.
lexico : Cmp.Comparator a -> Cmp.Comparator (List a)
lexico cmpElem xs ys =
    case (xs, ys) of
        (xHead :: xTail, yHead :: yTail) ->
            case cmpElem xHead yHead of
                EQ -> lexico cmpElem xTail yTail
                or -> or
        ([], _ :: _) -> LT
        (_ :: _, []) -> GT
        ([], []) -> EQ


-- | Maybe order.
maybe : Cmp.Comparator a -> Cmp.Comparator (Maybe a)
maybe cmp mx my =
    case (mx, my) of
        (Just x, Just y) -> cmp x y
        (Nothing, Just _) -> LT
        (Just _, Nothing) -> GT
        (Nothing, Nothing) -> EQ


consFst : a -> (List a, List b) -> (List a, List b)
consFst x (xs, ys) = (x :: xs, ys)


consSnd : b -> (List a, List b) -> (List a, List b)
consSnd y (xs, ys) = (xs, y :: ys)
