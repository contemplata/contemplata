module Model exposing
  ( Model, Window, NodeId, Node, Drag, Link, Addr, Focus(..)
  , selectWin, dragOn, getTree
  , getPosition, nextTree, prevTree, moveCursor
  , treeNum, treePos, selectNode, getLabel, setLabel
  , deleteSel, addSel
  -- Lenses:
  , top, bot, dim, winLens, drag, select, pos, height, heightProp )


import Mouse exposing (Position)
import Set as S
import Dict as D
import List as L
import Focus exposing ((=>))
import Focus as Focus
import Maybe as Maybe

import Util as Util
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

  -- , dragOn : Maybe (Focus, Drag)
  -- , dragOn : Maybe Focus

  -- links between the nodes
  , links : S.Set Link

  -- window dimensions
  , dim : Dim

--   -- size of the top window and proportion between the top/bottom sizes
--   , winHeight : Int
--   , winProp : Int
  }


type alias Dim =
  { width : Int
  , height : Int
  , heightProp : Int
  }


type alias Window =
  { tree : TreeId
  , select : S.Set NodeId
  , pos : Position
  , drag : Maybe Drag
  }


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
-- Misc
---------------------------------------------------


-- | Return the window in focus.
selectWin : Focus -> Model -> Window
selectWin focus model =
  case focus of
    Top -> model.top
    Bot -> model.bot


-- | On which window the drag is activated?
-- Return `Bot` if not activated.
dragOn : Model -> Focus
dragOn model =
  case model.top.drag of
    Just _ -> Top
    _ -> Bot
-- dragOn : Model -> Maybe Focus
-- dragOn model =
--   case (model.top.drag, model.bot.drag) of
--     (Just _, _)  -> Just Top
--     (_, Just _)  -> Just Bot
--     _ -> Nothing


-- | Get a tree under a given ID.
getTree : TreeId -> Model -> R.Tree Node
getTree tree model =
  case D.get tree model.trees of
    Nothing -> Debug.crash "Model.getTree: no tree with the given ID"
    Just t  -> t


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


-- getPosition : Focus -> Model -> Position
-- getPosition win model =
--   case (win, model.drag) of
--     (Top, Just (Top, {start, current})) ->
--       Position
--         (model.top.pos.x + current.x - start.x)
--         (model.top.pos.y + current.y - start.y)
--     (Top, _) -> model.top.pos
--     (Bot, Just (Bot, {start, current})) ->
--       Position
--         (model.bot.pos.x + current.x - start.x)
--         (model.bot.pos.y + current.y - start.y)
--     (Bot, _) -> model.bot.pos


getPosition : Window -> Position
getPosition win =
  case win.drag of
    Just {start, current} ->
      Position
        (win.pos.x + current.x - start.x)
        (win.pos.y + current.y - start.y)
    Nothing -> win.pos


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
getLabel nodeId focus model =
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
    tree = getTree (selectWin focus model).tree model
  in
    Maybe.withDefault "?" <| search tree


setLabel : NodeId -> Focus -> String -> Model -> Model
setLabel nodeId focus newLabel model =
  let
    update (R.Node x ts) = if nodeId == x.nodeId
      then R.Node {x | nodeVal = newLabel} ts
      else R.Node x (updateF ts)
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd :: updateF tl
    win = selectWin focus model
    tree = getTree win.tree model
    newTrees = D.insert win.tree (update tree) model.trees
  in
    {model | trees = newTrees}


---------------------------------------------------
-- Process selected
---------------------------------------------------


-- | Delete selected nodes in a given window.
procSel
  :  (S.Set NodeId -> R.Tree Node -> R.Tree Node)
  -> Focus -> Model -> Model
procSel f focus model =
  let
    win = selectWin focus model
    tree = getTree win.tree model
    newTree = f win.select tree
    newTrees = D.insert win.tree newTree model.trees
  in
    updateSelect focus <| { model | trees = newTrees }


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
    split3 ts =
      let
        (ls, tl) = Util.split (\x -> S.member (rootId x) ids) ts
        (ms, rs) = Util.split (\x -> not <| S.member (rootId x) ids) tl
      in
        (ls, ms, rs)
    update (R.Node x ts) =
      let
        (ls, ms, rs) = split3 ts
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
          <| Util.catMaybes
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



---------------------------------------------------
-- Lenses
---------------------------------------------------


top : Focus.Focus { record | top : a } a
top = Focus.create
  .top
  (\fn model -> {model | top = fn model.top})


bot : Focus.Focus { record | bot : a } a
bot = Focus.create
  .bot
  (\fn model -> {model | bot = fn model.bot})


winLens : Focus -> Focus.Focus { record | bot : a, top : a } a
winLens focus =
  case focus of
    Top -> top
    Bot -> bot


dim : Focus.Focus { record | dim : a } a
dim = Focus.create
  .dim
  (\fn model -> {model | dim = fn model.dim})


select : Focus.Focus { record | select : a } a
select = Focus.create
  .select
  (\fn model -> {model | select = fn model.select})


pos : Focus.Focus { record | pos : a } a
pos = Focus.create
  .pos
  (\fn model -> {model | pos = fn model.pos})


drag : Focus.Focus { record | drag : a } a
drag = Focus.create
  .drag
  (\fn model -> {model | drag = fn model.drag})


height : Focus.Focus { record | height : a } a
height = Focus.create
  .height
  (\fn model -> {model | height = fn model.height})


heightProp : Focus.Focus { record | heightProp : a } a
heightProp = Focus.create
  .heightProp
  (\fn model -> {model | heightProp = fn model.heightProp})
