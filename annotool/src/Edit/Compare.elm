module Edit.Compare exposing
  ( compareSyntax
  )


import Basics exposing (Order(..))
import Set as S
import Compare as Cmp

import Rose as R

import Edit.Core exposing (..)
import Edit.Anno as Anno
import Edit.Model as M


---------------------------------------------------
-- Top-level
---------------------------------------------------


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
compareSyntax : R.Tree M.Node -> R.Tree M.Node -> (List NodeId, List NodeId)
compareSyntax tx ty =
    let infos = List.sortWith compInfo << R.flatten << extractInfo
    in  diffInfo (infos tx) (infos ty)


-- | Compare two, ordered lists of infos.  The resulting pair (xs, ys) contains
-- the missing node IDs in the two respective input lists.
diffInfo : List Info -> List Info -> (List NodeId, List NodeId)
diffInfo xs ys =
    case (xs, ys) of
        (xHead :: xTail, yHead :: yTail) ->
            case compInfo xHead yHead of
                EQ -> diffInfo xTail yTail
                LT -> consFst xHead.id <| diffInfo xTail ys
                GT -> consSnd yHead.id <| diffInfo xs yTail
        (xHead :: xTail, []) ->
            consFst xHead.id <| diffInfo xTail []
        ([], yHead :: yTail) ->
            consSnd yHead.id <| diffInfo [] yTail
        ([], []) -> ([], [])


-- | Information about a node important for comparison.
type alias Info =
    { id : NodeId
    -- ^ ID of the node
    , label : String
    -- ^ The label of the node
    , comment : String
    -- ^ The comment of the node
    , annoTyp : Maybe M.NodeAnnoTyp
    -- ^ Node annotation
    , spans : List (S.Set Int)
    -- ^ The set of positions in the leaves of the subsequent subtrees in the
    -- tree corresponding to the node
    }


-- | Merge the list of span sets.
merge : List (S.Set Int) -> S.Set Int
merge xs =
    case xs of
        head :: tail -> S.union head (merge tail)
        [] -> S.empty


-- | Extract the list of node information from the tree.
extractInfo : R.Tree M.Node -> R.Tree Info
extractInfo (R.Node x ts) =
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
                children = List.map extractInfo ts
                value =
                    { id = r.nodeId
                    , comment = r.nodeComment
                    , annoTyp = r.nodeTyp
                    , label = r.nodeVal
                    , spans = List.map (merge << .spans << R.label) children
                    }
            in
                R.Node value children


---------------------------------------------------
-- Info comparison
---------------------------------------------------


-- | Compare two information values.
compInfo : Cmp.Comparator Info
compInfo =
    Cmp.concat
        [ Cmp.by .label
        , Cmp.by .comment
        , Cmp.compose .annoTyp (maybe compAnnoTyp)
        , Cmp.compose .spans compSpanList ]


compSpanList : Cmp.Comparator (List (S.Set Int))
compSpanList = lexico compSpanSet


compSpanSet : Cmp.Comparator (S.Set Int)
compSpanSet =
    Cmp.compose S.toList (lexico Basics.compare)


compAnnoTyp : Cmp.Comparator M.NodeAnnoTyp
compAnnoTyp annoX annoY =
    case (annoX, annoY) of
        (M.NodeEvent evX, M.NodeEvent evY) ->
            -- Well, yes, we should not probably use `toString` here, but
            -- really, it's too much trouble to define the comparison function
            -- for more complex types...
            Basics.compare (toString evX) (toString evY)
        (M.NodeEvent _, _) -> LT
        (_, M.NodeEvent _) -> GT
        (M.NodeSignal siX, M.NodeSignal siY) ->
            Basics.compare (toString siX) (toString siY)
        (M.NodeSignal _, _) -> LT
        (_, M.NodeSignal _) -> GT
        (M.NodeTimex tiX, M.NodeTimex tiY) ->
            Basics.compare (toString tiX) (toString tiY)


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
