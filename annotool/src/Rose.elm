module Rose exposing
  ( Tree(..), Forest, Width, leaf, withWidth, getRootSnd
  , map, flatten, mapAccum, getSubTree )


import Util

import List as List


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


mapAccumL : (acc -> a -> (acc, b)) -> acc -> List a -> (acc, List b)
mapAccumL f acc xs =
  case xs of
    [] -> (acc, [])
    x :: tl ->
      let
        (acc1, y) = f acc x
        (acc2, ys) = mapAccumL f acc1 tl
      in
        (acc2, y :: ys)


-- | Find the first subtree which satisfies the given predicate.
getSubTree : (a -> Bool) -> Tree a -> Maybe (Tree a)
getSubTree p t =
  let
    goT (Node x ts) =
      if p x
      then Just (Node x ts)
      else goF ts
    goF ts = List.foldl Util.or Nothing (List.map goT ts)
  in
    goT t


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
      ts = List.map (withWidth f margin) subTrees
      ws = List.map getRootSnd ts
      width = max (List.sum ws) (f x + margin)
    in
      Node (x, width) ts


-- | Retrieve the widgth stored in the root.
getRootSnd : Tree (a, b) -> b
getRootSnd (Node (x, w) _) = w
