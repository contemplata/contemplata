module Edit.Rule exposing
  ( Rule
  , theRule
  , apply
  -- , Context
  )

import Maybe
import Set as S
import Focus as Lens
import Lazy
import Lazy exposing (Lazy)

import Util
import Rose as R
import Edit.Model as M
import Edit.Core as C


------------------------------------------------------------
-- NEW
------------------------------------------------------------


-- | A rule on the top-most level of the given tree.
type alias Rule
    = C.NodeId -- ^ The new, unused node ID
    -> R.Tree M.Node
    -> Maybe (R.Tree M.Node, S.Set C.NodeId)


-- | Apply the given rule once, over the entire tree.
applyOnce : Rule -> C.NodeId -> R.Tree M.Node -> (R.Tree M.Node, S.Set C.NodeId)
applyOnce rule freeID tree =
    Maybe.withDefault (tree, S.empty) (rule freeID tree)


-- | Apply the rule over the entire tree and all its subtrees.
applyMany : Rule -> C.NodeId -> R.Tree M.Node -> (R.Tree M.Node, S.Set C.NodeId)
applyMany rule freeID0 (R.Node x subTrees) =
    let
        onChild freeID subTree =
            let (newSubTree, select) = applyMany rule freeID subTree
                newFreeID = max freeID (maxID newSubTree + 1)
            in  (newFreeID, (newSubTree, select))
        (freeID1, result) = Util.mapAccumL onChild freeID0 subTrees
        (newSubTrees, newIdList) = List.unzip result
        newIds1 = Util.unions newIdList
        (newTree, newIds2) = applyOnce rule freeID1 (R.Node x newSubTrees)
    in
        (newTree, S.union newIds1 newIds2)


-- | Apply the rule over the entire tree and all its subtrees.
apply : Rule -> R.Tree M.Node -> (R.Tree M.Node, S.Set C.NodeId)
apply rule tree =
    let freeID = maxID tree + 1
    in  applyMany rule freeID tree


-- | Merge the list of rules into a single rule.
merge : List Rule -> Rule
merge rules freeID tree = case rules of
  [] -> Nothing
  (r :: rs) -> case r freeID tree of
    Nothing -> merge rs freeID tree
    Just v  -> Just v


------------------------------------------------------------
-- Pattern
------------------------------------------------------------


type Pattern
    = Epsilon
      -- ^ Or `True`
    | Pred (R.Tree M.Node -> Bool)
      -- ^ Tree predicate
    | Seq Pattern (Lazy Pattern)
      -- ^ Sequential ordering: first one pattern, then the other one
    -- | Star Pattern
    | Or Pattern Pattern


-- | Non-lazy (I believe?) `Seq`.
seq : Pattern -> Pattern -> Pattern
seq x y = Seq x <| Lazy.lazy <| always y


-- | One or more patterns.  Greedy.
plus : Pattern -> Pattern
plus pat =
    Seq pat <| Lazy.lazy (\() -> Or (star pat) Epsilon)


-- | Zero or more patterns.  Greedy.
star : Pattern -> Pattern
star pat = Or (plus pat) Epsilon


-- | Optional pattern
optional : Pattern -> Pattern
optional pat = Or pat Epsilon


-- | Verify the root label.
root : String -> Pattern
root x = rootSat <| \y -> x == y


-- | Verify the root label.
rootSat : (String -> Bool) -> Pattern
rootSat pred =
    Pred <|
        \tree ->
            pred <| Lens.get M.nodeVal (R.label tree)


-- | Match any given single tree.
any : Pattern
any = rootSat <| always True


-- | Match the given list of labels against the prefix of the given list of
-- trees.
match
    : Pattern
    -> R.Forest M.Node
    -> Maybe (R.Forest M.Node, R.Forest M.Node)
match pat trees =
    case (pat, trees) of
        (Epsilon, _) -> Just ([], trees)
        (Pred p, t :: ts) ->
            if p t
            then Just ([t], ts)
            else Nothing
        (Seq pat1 pat2, _) ->
            case match pat1 trees of
                Nothing -> Nothing
                Just (xs1, ys1) ->
                    case match (Lazy.force pat2) ys1 of
                        Nothing -> Nothing
                        Just (xs2, ys2) -> Just (xs1 ++ xs2, ys2)
        (Or pat1 pat2, _) ->
            case match pat1 trees of
                Nothing -> match pat2 trees
                Just re -> Just re
        _ -> Nothing


------------------------------------------------------------
-- Contextual rules
------------------------------------------------------------


type alias ContextRule =
    { parent : String -> Bool
      -- ^ What should be the label of the parent node.
    , left : Pattern
    , middle : Pattern
      -- ^ The part that should actually match
    , right : Pattern
    , result : String
    }


-- | Compile a context rule to an actual rule.
compile
    : ContextRule
    -> Rule
compile cxt freeID tree =
    Util.guard (cxt.parent <| Lens.get M.nodeVal <| R.label tree)
       |> Maybe.andThen (\_ -> match cxt.left <| R.subTrees tree)
       |> Maybe.andThen (\(left, leftRest) ->
--                              let
--                                  x = Debug.log "left" left
--                                  y = Debug.log "leftRest" leftRest
--                              in
                                 match cxt.middle leftRest
       |> Maybe.andThen (\(middle, middleRest) ->
--                              let
--                                  x = Debug.log "middle" middle
--                              in
                                 match cxt.right middleRest
       |> Maybe.andThen (\(_, _) ->
            let
                newNode = M.Node
                    { nodeId = freeID
                    , nodeVal = cxt.result
                    , nodeTyp = Nothing
                    , nodeComment = "" }
                newMiddle = R.Node newNode middle
                newSubTrees = left ++ [newMiddle] ++ middleRest
                newRoot = R.label tree
            in
                Just (R.Node newRoot newSubTrees, S.singleton freeID)
       )))


------------------------------------------------------------
-- Rules
------------------------------------------------------------


-- | The list of flattening rules.
allRules : List Rule
allRules =
    [ compile
          { parent = \x ->
                List.member x ["Ssub"]
          , left = seq
              -- (star <| rootSat <| \label -> label /= "CS")
              (optional <| root "PUNC")
              (root "CS")
          , middle = star any
          , right = Epsilon
          , result = "Sint" }
--     , compile
--           { parent = \x ->
--                 List.member x ["SENT", "VPinf", "Srel"]
--           , left = star <| rootSat <| \label -> label /= "VN"
--           , middle = seq (root "VN") (root "VPinf")
--           , right = Epsilon -- star <| rootSat <| always True
--           , result = "VP" }
--     , compile
--           { parent = \x ->
--                 List.member x ["VPinf", "Ssub"]
--           , left = Epsilon
--           , middle = seq (root "VN") (root "NP")
--           , right = Epsilon
--           , result = "VP" }
    ]


-- | "The" rule is the list of all rules compiled into one.
theRule : Rule
theRule = merge allRules


------------------------------------------------------------
-- Rules
------------------------------------------------------------


-- -- | The list of flattening rules.
-- allRules : List Rule
-- allRules =
--   let
--     mkRule v p x = if p x then Just v else Nothing
--   in
--     [ mkRule "VP" (\x ->
--         List.member x.parent ["SENT", "VPinf", "Srel"] &&
--         x.left == "VN" &&
--         x.right == "VPinf")
--     , mkRule "VP" (\x ->
--         List.member x.parent ["SENT", "Ssub", "COORD", "Sint"] &&
--         x.left == "VN" &&
--         x.right == "NP")
--     , mkRule "VP" (\x ->
--         List.member x.parent ["VPinf", "Ssub"] &&
--         x.left == "VN" &&
--         x.right == "PP")
--     , mkRule "VP" (\x ->
--         List.member x.parent ["SENT"] &&
--         x.left == "VN" &&
--         x.right == "ADV")
--     ]
--
--
-- -- | Compile the list of rules into a single rule.
-- compile : List Rule -> Rule
-- compile rules ctx = case rules of
--   [] -> Nothing
--   (r :: rs) -> case r ctx of
--     Nothing -> compile rs ctx
--     Just v  -> Just v
--
--
-- -- | "The" rule is the list of all rules compiled into one.
-- theRule : Rule
-- theRule = compile allRules
--
--
-- ------------------------------------------------------------
-- -- Rule types
-- ------------------------------------------------------------
--
--
-- type alias Context =
--     { parent : String
--     , left : List String
--     , match : List String
--     , right : List String
--       -- ^ All three lists should be non-empty
--     }
--
--
-- -- | If the rule returns `Just`, a new node (with the resulting label) can be
-- -- added over `left` and `right`, and below the `parent`.
-- type alias Rule = Context -> Maybe String
--
--
-- ------------------------------------------------------------
-- -- Application
-- ------------------------------------------------------------
--
--
-- -- | Apply the rule everywhere when it can be applied in the given tree, and
-- -- return the set of the IDs of the new nodes.
-- apply : Rule -> R.Tree M.Node -> (R.Tree M.Node, S.Set C.NodeId)
-- apply rule tree0 =
--   let
--     go tree curId = case tree of
--       R.Node root [] ->
--         { result = R.Node root []
--         , nodeSet = S.empty
--         , curId = curId }
--       R.Node root (x :: []) ->
--         let r = go x curId
--         in  {r | result = R.Node root (r.result :: [])}
--       R.Node root (x :: y :: xs) ->
--         let
--           value = Lens.get M.nodeVal
--           rootVal = value << R.label
--           context =
--             { parent = value root
--             , left   = rootVal x
--             , right  = rootVal y }
--         in
--           case rule context of
--             Nothing -> joinFirst (go x) (go (R.Node root (y :: xs))) curId
--             Just newLabel ->
--               let
--                 newNode = M.Node
--                   { nodeId = curId
--                   , nodeVal = newLabel
--                   , nodeTyp = Nothing
--                   , nodeComment = "" }
--               in
--                 joinFirst
--                   (addRoot newNode (go x) (go y))
--                   (go (R.Node root xs))
--                   (curId + 1)
--   in
--     (\r -> (r.result, r.nodeSet))
--     <| go tree0
--     <| (\i -> i+1)
--     <| maxID tree0
--
--
-- -- | The result of computation, kind of. Normally, we would use a monad here...
-- type alias CompRes =
--   { result : R.Tree M.Node
--     -- ^ The resulting tree
--   , nodeSet : S.Set C.NodeId
--     -- ^ The resulting set of node IDs
--   , curId : C.NodeId
--     -- ^ The current maximum ID
--   }
--
--
-- -- | A computation, which takes the max ID and returns the computation result.
-- type alias Comp = C.NodeId -> CompRes
--
--
-- joinFirst
--      : Comp -- x
--     -> Comp -- r xs
--     -> Comp -- r (x : xs)
-- joinFirst f g curId =
--   let
--     x = f curId
--     y = g x.curId
--   in
--     case y.result of
--       R.Node root forest ->
--         { result = R.Node root (x.result :: forest)
--         , nodeSet = S.union x.nodeSet y.nodeSet
--         , curId = y.curId
--         }
--
--
-- addRoot
--      : M.Node -- x
--     -> Comp   -- y
--     -> Comp   -- z
--     -> Comp   -- x [y, z]
-- addRoot x f g curId =
--   let
--     y = f curId
--     z = g y.curId
--   in
--     { result = R.Node x [y.result, z.result]
--     , nodeSet = S.union y.nodeSet z.nodeSet
--              |> S.insert (Lens.get M.nodeId x)
--     , curId = z.curId
--     }


------------------------------------------------------------
-- Utils
------------------------------------------------------------


maxID : R.Tree M.Node -> C.NodeId
maxID =
  let id = Lens.get M.nodeId
  in  Maybe.withDefault 0 << List.maximum << List.map id << R.flatten
