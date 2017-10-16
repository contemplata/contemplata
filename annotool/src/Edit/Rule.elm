module Edit.Rule exposing
  ( Context
  , Rule
  , theRule
  , apply
  )

import Set as S
import Focus as Lens

import Util
import Rose as R
import Edit.Model as M
import Edit.Core as C


------------------------------------------------------------
-- Rules
------------------------------------------------------------


-- | The list of flattening rules.
allRules : List Rule
allRules =
  let
    mkRule v p x = if p x then Just v else Nothing
  in
    [ mkRule "VP" (\x ->
        List.member x.parent ["SENT", "VPinf", "Srel"] &&
        x.left == "VN" &&
        x.right == "VPinf")
    , mkRule "VP" (\x ->
        List.member x.parent ["SENT", "Ssub", "COORD", "Sint"] &&
        x.left == "VN" &&
        x.right == "NP")
    , mkRule "VP" (\x ->
        List.member x.parent ["VPinf", "Ssub"] &&
        x.left == "VN" &&
        x.right == "PP")
    , mkRule "VP" (\x ->
        List.member x.parent ["SENT"] &&
        x.left == "VN" &&
        x.right == "ADV")
    ]


-- | Compile the list of rules into a single rule.
compile : List Rule -> Rule
compile rules ctx = case rules of
  [] -> Nothing
  (r :: rs) -> case r ctx of
    Nothing -> compile rs ctx
    Just v  -> Just v


-- | "The" rule is the list of all rules compiled into one.
theRule : Rule
theRule = compile allRules


------------------------------------------------------------
-- Rule types
------------------------------------------------------------


type alias Context =
    { parent : String
    , left : String
    , right : String
    }


-- | If the rule returns `Just`, a new node (with the resulting label) can be
-- added over `left` and `right`, and below the `parent`.
type alias Rule = Context -> Maybe String


------------------------------------------------------------
-- Application
------------------------------------------------------------


-- | Apply the rule everywhere when it can be applied in the given tree, and
-- return the set of the IDs of the new nodes.
apply : Rule -> R.Tree M.Node -> (R.Tree M.Node, S.Set C.NodeId)
apply rule tree0 =
  let
    go tree curId = case tree of
      R.Node root [] ->
        { result = R.Node root []
        , nodeSet = S.empty
        , curId = curId }
      R.Node root (x :: []) ->
        let r = go x curId
        in  {r | result = R.Node root (r.result :: [])}
      R.Node root (x :: y :: xs) ->
        let
          value = Lens.get M.nodeVal
          rootVal = value << R.label
          context =
            { parent = value root
            , left   = rootVal x
            , right  = rootVal y }
        in
          case rule context of
            Nothing -> joinFirst (go x) (go (R.Node root (y :: xs))) curId
            Just newLabel ->
              let
                newNode = M.Node
                  { nodeId = curId
                  , nodeVal = newLabel
                  , nodeTyp = Nothing
                  , nodeComment = "" }
              in
                joinFirst
                  (addRoot newNode (go x) (go y))
                  (go (R.Node root xs))
                  (curId + 1)
  in
    (\r -> (r.result, r.nodeSet))
    <| go tree0
    <| (\i -> i+1)
    <| maxID tree0


-- | The result of computation, kind of. Normally, we would use a monad here...
type alias CompRes =
  { result : R.Tree M.Node
    -- ^ The resulting tree
  , nodeSet : S.Set C.NodeId
    -- ^ The resulting set of node IDs
  , curId : C.NodeId
    -- ^ The current maximum ID
  }


-- | A computation, which takes the max ID and returns the computation result.
type alias Comp = C.NodeId -> CompRes


joinFirst
     : Comp -- x
    -> Comp -- r xs
    -> Comp -- r (x : xs)
joinFirst f g curId =
  let
    x = f curId
    y = g x.curId
  in
    case y.result of
      R.Node root forest ->
        { result = R.Node root (x.result :: forest)
        , nodeSet = S.union x.nodeSet y.nodeSet
        , curId = y.curId
        }


addRoot
     : M.Node -- x
    -> Comp   -- y
    -> Comp   -- z
    -> Comp   -- x [y, z]
addRoot x f g curId =
  let
    y = f curId
    z = g y.curId
  in
    { result = R.Node x [y.result, z.result]
    , nodeSet = S.union y.nodeSet z.nodeSet
             |> S.insert (Lens.get M.nodeId x)
    , curId = z.curId
    }


maxID : R.Tree M.Node -> C.NodeId
maxID =
  let id = Lens.get M.nodeId
  in  Maybe.withDefault 0 << List.maximum << List.map id << R.flatten
