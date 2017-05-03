module Model exposing
  ( Model, NodeId, Node, Drag, Link, Addr, Focus(..)
  , getPosition, nextTree, prevTree, moveCursor
  , treeNum, treePos, selectNode, getLabel, setLabel
  , deleteSel, addSel
  , top, bot, drag, select, pos )


import Mouse exposing (Position)
import Set as S
import Dict as D
import List as L
import Focus exposing ((=>))
import Focus as Focus
import Maybe as Maybe

import Rose as R


---------------------------------------------------
-- Types
---------------------------------------------------


type alias Model =
  { trees : D.Dict TreeId (R.Tree Node)

  , top : Window
  , bot : Window

  -- which window is the focus on
  , focus : Focus

  , drag : Maybe (Focus, Drag)

  -- links between the nodes
  , links : S.Set Link

  -- size of the top window and proportion between the top/bottom sizes
  , winHeight : Int
  , winProp : Int
  }


top : Focus.Focus { record | top : a } a
top = Focus.create
  .top
  (\fn model -> {model | top = fn model.top})


bot : Focus.Focus { record | bot : a } a
bot = Focus.create
  .bot
  (\fn model -> {model | bot = fn model.bot})


drag : Focus.Focus { record | drag : a } a
drag = Focus.create
  .drag
  (\fn model -> {model | drag = fn model.drag})


type alias Window =
  { tree : TreeId
  , pos : Position
  , select : S.Set NodeId
  }


select : Focus.Focus { record | select : a } a
select = Focus.create
  .select
  (\fn model -> {model | select = fn model.select})


pos : Focus.Focus { record | pos : a } a
pos = Focus.create
  .pos
  (\fn model -> {model | pos = fn model.pos})


-- -- | Link between two trees.
-- type alias Link =
--   { from : (TreeId, NodeId)
--   , to : (TreeId, NodeId) }


-- | Link between two trees.
type alias Link = (Addr, Addr)
type alias Addr = (TreeId, NodeId)


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


-- Focus selector
type Focus = Top | Bot


---------------------------------------------------
-- Position
---------------------------------------------------


treePos : Focus -> Model -> Int
treePos win model =
  let
    tree = case win of
      Top -> model.top.tree
      Bot -> model.bot.tree
    go i keys = case keys of
      [] -> 0
      hd :: tl -> if tree == hd
        then i
        else go (i + 1) tl
  in
    go 1 (D.keys model.trees)


treeNum : Model -> Int
treeNum model = D.size model.trees


getPosition : Focus -> Model -> Position
getPosition win model =
  case (win, model.drag) of
    (Top, Just (Top, {start, current})) ->
      Position
        (model.top.pos.x + current.x - start.x)
        (model.top.pos.y + current.y - start.y)
    (Top, _) -> model.top.pos
    (Bot, Just (Bot, {start, current})) ->
      Position
        (model.bot.pos.x + current.x - start.x)
        (model.bot.pos.y + current.y - start.y)
    (Bot, _) -> model.bot.pos


-- getPosition : Focus -> Model -> Position
-- getPosition win model =
--   let
--     pos = case win of
--       Top -> model.top.pos
--       Bot -> model.bot.pos
--   in
--     case model.drag of
--       Just (Top, {start, current}) ->
--         { x = pos.x + current.x - start.x
--         , y = pos.y + current.y - start.y }
--       _ -> pos


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
  let
    switch = if next then nextTree else prevTree
  in
  case model.focus of
    Top  ->
      let modelTop = model.top in
      { model |
          top = { modelTop
            | tree = switch model.top.tree model
            , select = S.empty }
      }
          -- | topTree = nextTree model.topTree model
--       { model
--           | topTree = nextTree model.topTree model
--           , topSelect = S.empty }
    Bot  ->
      let modelBot = model.bot in
      { model |
          bot = { modelBot
            | tree = switch model.bot.tree model
            , select = S.empty }
      }


---------------------------------------------------
-- Select
---------------------------------------------------


-- We bypass the focus of the model since the node can be possibly selected
-- before the window it is in is even focused on!
selectNode : Focus -> NodeId -> Model -> Model
selectNode win i model =
  let
    update sel = case S.member i sel of
      True  -> S.remove i sel
      False -> S.insert i sel
  in
    case win of
      Top -> Focus.update (top => select) update model
      Bot -> Focus.update (bot => select) update model


---------------------------------------------------
-- Labels
---------------------------------------------------


getLabel : NodeId -> Focus -> Model -> String
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
      Top -> D.get model.top.tree model.trees
      Bot -> D.get model.bot.tree model.trees
  in
    Maybe.withDefault "?" <|
      Maybe.andThen search tree


setLabel : NodeId -> Focus -> String -> Model -> Model
setLabel nodeId win newLabel model =
  let
    update (R.Node x ts) = if nodeId == x.nodeId
      then R.Node {x | nodeVal = newLabel} ts
      else R.Node x (updateF ts)
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd :: updateF tl
    tree = case win of
      Top -> D.get model.top.tree model.trees
      Bot -> D.get model.bot.tree model.trees
    newTrees = case (win, tree) of
      (_, Nothing)  -> model.trees
      (Top, Just t) -> D.insert model.top.tree (update t) model.trees
      (Bot, Just t) -> D.insert model.bot.tree (update t) model.trees
  in
    {model | trees = newTrees}


---------------------------------------------------
-- Process selected
---------------------------------------------------


-- | Delete selected nodes in a given window.
procSel
  :  (S.Set NodeId -> R.Tree Node -> R.Tree Node)
  -> Focus -> Model -> Model
procSel f win model =
  let
    selected = case win of
      Top -> model.top.select
      Bot -> model.bot.select
    tree = case win of
      Top -> D.get model.top.tree model.trees
      Bot -> D.get model.bot.tree model.trees
  in
    case tree of
      Nothing -> model
      Just t  ->
        let
          newTree = f selected t
          newTrees = case win of
            Top -> D.insert model.top.tree newTree model.trees
            Bot -> D.insert model.bot.tree newTree model.trees
        in
          updateSelect win <| { model | trees = newTrees }

---------------------------------------------------
-- Delete
---------------------------------------------------


-- | Delete selected nodes in a given window.
deleteSel : Focus -> Model -> Model
deleteSel =
  let f ids t = L.foldl deleteNode t (S.toList ids)
  in  procSel f


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


-- | Add selected nodes in a given window.
addSel : Focus -> Model -> Model
addSel = procSel addNode


-- | Add a parent to a given node.
addNode : S.Set NodeId -> R.Tree Node -> R.Tree Node
addNode ids tree =
  let
    rootId (R.Node x _) = x.nodeId
    split ts =
      let
        (ls, tl) = find (\x -> S.member (rootId x) ids) ts
        (ms, rs) = find (\x -> not <| S.member (rootId x) ids) tl
      in
        (ls, ms, rs)
    update (R.Node x ts) =
      let
        (ls, ms, rs) = split ts
      in
        if L.isEmpty ms
          then R.Node (Just x) (updateF ts)
          else R.Node (Just x)
            (  updateF ls
            ++ [R.Node Nothing (updateF ms)]
            ++ updateF rs )
    updateF = L.map update
  in
    identify "?" <| update tree


-- | Add missing identifiers.
identify : String -> R.Tree (Maybe Node) -> R.Tree Node
identify dummyVal tree =
  let
    findMaxMay =
        List.maximum
          <| L.map (\x -> x.nodeId)
          <| catMaybes
          <| R.flatten tree
    newId1 = case findMaxMay of
       Nothing -> 1
       Just ix -> ix + 1
    update newId nodeMay =
      case nodeMay of
        Nothing -> (newId+1, {nodeId=newId, nodeVal=dummyVal})
        Just x  -> (newId, x)
  in
    Tuple.second <| R.mapAccum update newId1 tree


---------------------------------------------------
-- Utils
---------------------------------------------------


-- | Update the set of the selected nodes depending on the window in which the
-- tree was modified.
updateSelect : Focus -> Model -> Model
updateSelect win model =
  let
    newTopSel = case win of
      Top -> S.empty
      Bot -> if model.top.tree == model.bot.tree
             then S.empty else model.top.select
    newBotSel = case win of
      Bot -> S.empty
      Top -> if model.top.tree == model.bot.tree
             then S.empty else model.bot.select
  in
    model
      |> Focus.set (top => select) newTopSel
      |> Focus.set (bot => select) newBotSel
--     { model
--         | top = { model.top | select = newTopSel }
--         , bot = { model.bot | select = newBotSel } }



-- | The results is (ls, rs) where `ls` are all the left-most elements which do
-- | not satisfy the given predicate, while `rs` is the rest. In particular, the
-- | first element of `rs` satisfies the predicate.
find : (a -> Bool) -> List a -> (List a, List a)
find p xs =
  let
    ys = case xs of
      [] -> ([], [])
      hd :: tl ->
        if p hd
          then ([], xs)
          else
            let (ls, rs) = find p tl
            in  (hd :: ls, rs)
     -- revFst (x, y) = (List.reverse x, y)
  in
    -- revFst ys
    ys


catMaybes : List (Maybe a) -> List a
catMaybes xs =
  case xs of
    [] -> []
    hd :: tl ->
      case hd of
        Nothing -> catMaybes tl
        Just x  -> x :: catMaybes tl
