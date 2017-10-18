module Rose exposing
  ( Tree(..), Forest, Width, leaf, label, subTrees
  , withWidth --, getRootSnd
  , map, flatten, mapAccum
  , getSubTree, delSubTree, putSubTree, swapSubTree
  , sortTree
  -- JSON
  , treeDecoder, encodeTree
  )


import Util
import Tuple exposing (first, second)
import Json.Decode as Decode
import Json.Encode as Encode

import List as L
import Util exposing (mapAccumL)


---------------------------------------------------
-- Rose trees
---------------------------------------------------


-- | A rose tree.
type Tree a = Node a (Forest a)


-- | A rose forest.
type alias Forest a = List (Tree a)


-- | Create a leaf.
leaf : a -> Tree a
leaf x = Node x []


-- | Get the label in the root.
label : Tree a -> a
label (Node x _) = x


-- | The list of subtrees.
subTrees : Tree a -> List (Tree a)
subTrees (Node _ xs) = xs


-- -- | Find the first occurence of a node which satisfies a given predicate.
-- find : (a -> Bool) -> Tree a -> Maybe a
-- find p (Node x ts) =
--   let
--     (acc1, y) = f acc x
--     (acc2, ys) = mapAccumL (mapAccum f) acc1 ts
--   in
--     case p x of
--       True -> Just x
--       False -> List.


map : (a -> b) -> Tree a -> Tree b
map f t =
  let
    g acc x = (acc, f x)
  in
    Tuple.second <| mapAccum g 0 t


flatten : Tree a -> List a
flatten t =
  let g acc x = (x :: acc, x)
  in  Tuple.first <| mapAccum g [] t


mapAccum : (acc -> a -> (acc, b)) -> acc -> Tree a -> (acc, Tree b)
mapAccum f acc (Node x ts) =
  let
    (acc1, y) = f acc x
    (acc2, ys) = mapAccumL (mapAccum f) acc1 ts
  in
    (acc2, Node y ys)


-- mapAccumF : (acc -> a -> (acc, b)) -> acc -> Forest a -> (acc, Forest b)
-- mapAccumF f acc (Node x ts) =
--   let
--     (acc1, y) = f acc x
--     (acc2, ts1) = List.foldl


-- | Find the first subtree which satisfies the given predicate.
getSubTree : (a -> Bool) -> Tree a -> Maybe (Tree a)
getSubTree p t =
  let
    goT (Node x ts) =
      if p x
      then Just (Node x ts)
      else goF ts
    goF ts = L.foldl Util.mappend Nothing (L.map goT ts)
  in
    goT t


-- | Returns the tree without the tree identified by `getSubTree`.
delSubTree : (a -> Bool) -> Tree a -> Maybe (Tree a)
delSubTree p t =
  let
    goT (Node x ts) =
      if p x
      then Nothing
      else Just (Node x (goF ts))
    goF ts = Util.catMaybes (L.map goT ts)
  in
    goT t


-- | Put the first tree under the node of the second tree satisfying the given
-- predicate.
putSubTree : Tree a -> (a -> Bool) -> Tree a -> Tree a
putSubTree s p t =
  let
    goT (Node x ts) = Node x <|
      if p x then s :: ts else goF ts
    goF ts = L.map goT ts
  in
    goT t


-- | Shift the subtree attached at each(?) node satisfying the given
-- predicate.  Normally, only one node should satisfy the predicate.
swapSubTree
  : Bool -- ^ Left or right?
  -> (a -> Bool)
  -> Tree a
  -> Tree a
swapSubTree left p t =
  let
    goT (Node x ts) = Node x <| goF <| L.map goT ts
    goF ts = case ts of
      x :: y :: tl ->
--         if left && p (label y)
--         then y :: goF (x :: tl)
--         else if not left && p (label x)
        if (left && p (label y)) || (not left && p (label x))
        then y :: x :: goF tl
        else x :: goF (y :: tl)
      _ -> ts
  in
    goT t


---------------------------------------------------
-- Sorting
---------------------------------------------------


-- -- | Sort the tree based on the ints assigned to its leaves.
-- sortTree : (a -> Int) -> Tree a -> Tree a
-- sortTree f t =
--   let
--     goT (Node x ts) =
--       case ts of
--         [] -> (Node x ts, f x)
--         _ ->
--           let
--             tps = L.sortBy second <| L.map goT ts
--             ts1 = L.map first tps
--             pos = round <| Util.average <| L.map (toFloat << second) tps
--           in
--             (Node x ts1, pos)
--   in
--     first <| goT t


-- | Sort the tree based on the ints assigned to its leaves.
sortTree
   : (a -> Int)  -- ^ Positions assigned to leaves
  -> (a -> Bool) -- ^ Determines the nodes whose children will be sorted
  -> Tree a
  -> Tree a
sortTree leafPos active t =
  let
    goT (Node x ts) =
      case ts of
        [] -> (Node x ts, leafPos x)
        _ ->
          let
            sort = if active x then L.sortBy second else identity
            tps = sort <| L.map goT ts
            ts1 = L.map first tps
            pos = round <| Util.average <| L.map (toFloat << second) tps
          in
            (Node x ts1, pos)
  in
    first <| goT t


---------------------------------------------------
-- Width
---------------------------------------------------


type alias Width = Int


-- | Calculate the width of the individual subtrees. The first argument is used
-- to calculate the width of nodes.
withWidth
   : (a -> Width) -- ^ Width of a node
  -> Width -- ^ Additional margin
  -> Tree a
  -> Tree (a, Width)
withWidth f margin (Node x subTrees) = case subTrees of
  [] -> Node (x, f x + margin) []
  _  ->
    let
      ts = L.map (withWidth f margin) subTrees
      ws = L.map (label >> second) ts
      width = max (L.sum ws) (f x + margin)
    in
      Node (x, width) ts


-- -- | Retrieve the widgth stored in the root.
-- getRootSnd : Tree (a, b) -> b
-- getRootSnd (Node (x, w) _) = w


---------------------------------------------------
-- JSON
---------------------------------------------------


treeDecoder : Decode.Decoder a -> Decode.Decoder (Tree a)
treeDecoder elemDecoder =
  let
    forestDecoder =
      Decode.list
        (Decode.lazy
          (\_ -> treeDecoder elemDecoder)
        )
  in
    Decode.map2 Node
      (Decode.index 0 elemDecoder)
      (Decode.index 1 forestDecoder)



encodeTree : (a -> Encode.Value) -> Tree a -> Encode.Value
encodeTree encodeElem =
  let
    encodeForest = Encode.list
      << List.map (encodeTree encodeElem)
  in
    \(Node x subTrees) ->
      Encode.list
        [ encodeElem x
        , encodeForest subTrees ]
