module Model exposing
  ( Model, NodeId, Node, Drag, Window(..)
  , getPosition, nextTree, prevTree, moveCursor
  , treeNum, treePos, select, getLabel, setLabel
  , deleteSel )


import Mouse exposing (Position)
import Set as S
import Dict as D
import List as L
import Maybe as Maybe
import Rose as R


---------------------------------------------------
-- Types
---------------------------------------------------


type alias Model =
    { trees : D.Dict TreeId (R.Tree Node)
    , topTree : TreeId
    , botTree : TreeId

    , topPos : Position
    , botPos : Position
    , drag : Maybe (Window, Drag)

    -- which window the focus is on
    , focus : Window

    , topSelect : S.Set NodeId
    , botSelect : S.Set NodeId
    }


-- Node identifier
type alias NodeId = Int


-- Tree identifier
type alias TreeId = String


-- Node in a syntactic tree
type alias Node =
  { nodeId : NodeId
  , nodeVal : String }


-- Information about a drag
type alias Drag =
    { start : Position
    , current : Position
    }


-- Windows selector
type Window = Top | Bot


---------------------------------------------------
-- Position
---------------------------------------------------


treePos : Window -> Model -> Int
treePos win model =
  let
    tree = case win of
      Top -> model.topTree
      Bot -> model.botTree
    go i keys = case keys of
      [] -> 0
      hd :: tl -> if tree == hd
        then i
        else go (i + 1) tl
  in
    go 1 (D.keys model.trees)


treeNum : Model -> Int
treeNum model = D.size model.trees


getPosition : Window -> Model -> Position
getPosition win model =
  case (win, model.drag) of
    (Top, Just (Top, {start, current})) ->
      Position
        (model.topPos.x + current.x - start.x)
        (model.topPos.y + current.y - start.y)
    (Top, _) -> model.topPos
    (Bot, Just (Bot, {start, current})) ->
      Position
        (model.botPos.x + current.x - start.x)
        (model.botPos.y + current.y - start.y)
    (Bot, _) -> model.botPos


---------------------------------------------------
-- Cursor
---------------------------------------------------


-- | Retrieve the next tree in the underlying model.
-- Return the argument tree ID if not possible.
nextTree : TreeId -> Model -> TreeId
nextTree x model =
  let
    go keys = case keys of
      [] -> x
      hd1 :: []  -> x
      hd1 :: hd2 :: tl -> if x == hd1
        then hd2
        else go (hd2 :: tl)
  in
    go (D.keys model.trees)


-- | Retrieve the next tree in the underlying model.
-- Return the argument tree ID if not possible.
prevTree : TreeId -> Model -> TreeId
prevTree x model =
  let
    go keys = case keys of
      [] -> x
      hd1 :: []  -> x
      hd1 :: hd2 :: tl -> if x == hd2
        then hd1
        else go (hd2 :: tl)
  in
    go (D.keys model.trees)


-- | Wrapper for prevTree and nextTree.
moveCursor : Bool -> Model -> Model
moveCursor next model =
  case (next, model.focus) of
    (True, Top)  ->
      { model
          | topTree = nextTree model.topTree model
          , topSelect = S.empty }
    (False, Top) ->
      { model
          | topTree = prevTree model.topTree model
          , topSelect = S.empty }
    (True, Bot)  ->
      { model
          | botTree = nextTree model.botTree model
          , botSelect = S.empty }
    (False, Bot) ->
      { model
          | botTree = prevTree model.botTree model
          , botSelect = S.empty }


---------------------------------------------------
-- Select
---------------------------------------------------


-- We bypass the focus of the model since the node can be possibly selected
-- before the window it is in is even focused on!
select : Window -> NodeId -> Model -> Model
select win i model =
  case win of
    Top -> { model | topSelect =
      case S.member i model.topSelect of
        True  -> S.remove i model.topSelect
        False -> S.insert i model.topSelect }
    Bot -> { model | botSelect =
      case S.member i model.botSelect of
        True  -> S.remove i model.botSelect
        False -> S.insert i model.botSelect }


---------------------------------------------------
-- Labels
---------------------------------------------------


getLabel : NodeId -> Window -> Model -> String
getLabel nodeId win model =
  let
    search (R.Node x ts) = if nodeId == x.nodeId
      then Just x.nodeVal
      else searchF ts
    searchF ts = case ts of
      [] -> Nothing
      hd :: tl -> or (search hd) (searchF tl)
    or x y = case (x, y) of
      (Just v, _)  -> Just v
      (Nothing, v) -> v
    tree = case win of
      Top -> D.get model.topTree model.trees
      Bot -> D.get model.botTree model.trees
  in
    Maybe.withDefault "?" <|
      Maybe.andThen search tree


setLabel : NodeId -> Window -> String -> Model -> Model
setLabel nodeId win newLabel model =
  let
    update (R.Node x ts) = if nodeId == x.nodeId
      then R.Node {x | nodeVal = newLabel} ts
      else R.Node x (updateF ts)
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd :: updateF tl
    tree = case win of
      Top -> D.get model.topTree model.trees
      Bot -> D.get model.botTree model.trees
    newTrees = case (win, tree) of
      (_, Nothing)  -> model.trees
      (Top, Just t) -> D.insert model.topTree (update t) model.trees
      (Bot, Just t) -> D.insert model.botTree (update t) model.trees
  in
    {model | trees = newTrees}


---------------------------------------------------
-- Delete
---------------------------------------------------


-- | Delete selected nodes in a given window.
deleteSel : Window -> Model -> Model
deleteSel win model =
  let
    selected = S.toList <| case win of
      Top -> model.topSelect
      Bot -> model.botSelect
    tree = case win of
      Top -> D.get model.topTree model.trees
      Bot -> D.get model.botTree model.trees
  in
    case tree of
      Nothing -> model
      Just t  ->
        let
          newTree = L.foldl deleteNode t selected
          newTrees = case win of
            Top -> D.insert model.topTree newTree model.trees
            Bot -> D.insert model.botTree newTree model.trees
          newTopSel = case win of
            Top -> S.empty
            Bot -> if model.topTree == model.botTree
                   then S.empty else model.topSelect
          newBotSel = case win of
            Bot -> S.empty
            Top -> if model.topTree == model.botTree
                   then S.empty else model.botSelect
        in
          { model
              | trees = newTrees
              , topSelect = newTopSel
              , botSelect = newBotSel }


-- | Delete a given node, provided that it is not a root.
deleteNode : NodeId -> R.Tree Node -> R.Tree Node
deleteNode nodeId tree =
  let
    update (R.Node x ts) = if nodeId == x.nodeId
      then ts
      else [R.Node x (updateF ts)]
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd ++ updateF tl
  in
    case update tree of
      [hd] -> hd
      _ -> tree -- A situation which can occur if you delete a root


---------------------------------------------------
-- Add
---------------------------------------------------
