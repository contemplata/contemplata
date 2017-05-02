module Rose exposing
  (Tree(..), Forest, Width, leaf, withWidth, getWidth)


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


---------------------------------------------------
-- Width
---------------------------------------------------


type alias Width = Int


-- | Calculate the width of the individual subtrees. The first argument is used
-- to calculate the width of leaves.
withWidth : (a -> Width) -> Tree a -> Tree (a, Width)
withWidth f (Node x subTrees) = case subTrees of
  [] -> Node (x, f x) []
  _  ->
    let
      ts = List.map (withWidth f) subTrees
      ws = List.map getWidth ts
    in
      Node (x, List.sum ws) ts


-- | Retrieve the widgth stored in the root.
getWidth : Tree (a, Int) -> Int
getWidth (Node (x, w) _) = w
