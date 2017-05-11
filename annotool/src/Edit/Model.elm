module Edit.Model exposing
  (
  -- Data types:
    TreeMap, FileId, File, NodeId, TreeId, Node(..), Link, Addr
  , isNode, isLeaf
  -- Model types:
  , Model, Window, Drag, Focus(..)
  -- Other:
  , selectWin, dragOn, getTree, selAll
  , getPosition, nextTree, prevTree, moveCursor
  , treeNum, treePos
  -- Initialization:
  -- , mkEdit
  -- Labels:
  , getLabel, setLabel
  -- Node selection:
  , selectNode, selectNodeAux
  -- Links
  , connect -- LinkInfo
  -- Tree modifications:
  , attachSel, deleteSel, addSel
  -- Lenses:
  , top, bot, dim, winLens, drag, pos, height, heightProp
  , nodeId, nodeVal, trees
  -- Pseudo-lenses:
  , setTrees
  -- JSON decoding:
  , treeMapDecoder, fileDecoder, treeDecoder, nodeDecoder
  -- JSON encoding:
  , encodeFile
  )


import Mouse exposing (Position)

import Set as S
import Dict as D
import List as L
import Focus exposing ((=>))
import Focus as Lens
import Maybe as Maybe

import Json.Decode as Decode
import Json.Encode as Encode
-- import Json.Decode.Pipeline as DePipe

import Util as Util
import Rose as R


---------------------------------------------------
-- Data types
---------------------------------------------------


type alias FileId = String


type alias File =
  { treeMap : TreeMap
  , linkSet : S.Set Link }


type alias TreeMap = D.Dict TreeId (R.Tree Node)


-- | Link between two trees.
type alias Link = (Addr, Addr)
type alias Addr = (TreeId, NodeId)


-- | Tree identifier
type alias TreeId = String


-- | Internal node identifier
type alias NodeId = Int


-- -- | Leaf identifier
-- type alias LeafId = Int


-- | Node in a syntactic tree is either an internal node or a leaf.
type Node
  = Node
    { nodeId : NodeId
    , nodeVal : String }
  | Leaf
    { nodeId : NodeId
    , nodeVal : String
      -- | The position of the leaf in the underlying sentence.
      -- The positions do not have to be consecutive.
    , leafPos : Int }


isNode : Node -> Bool
isNode x = case x of
  Node _ -> True
  _ -> False


isLeaf : Node -> Bool
isLeaf = not << isNode


-- | Verify the basic well-formedness properties.
wellFormed : R.Tree Node -> Bool
wellFormed (R.Node x ts) =
  case ts of
    [] -> isLeaf x
    _  -> isNode x && Util.and (L.map wellFormed ts)


---------------------------------------------------
-- Model-related types
---------------------------------------------------


type alias Model =
  { fileId : FileId

  , trees : TreeMap

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

  -- is CTRL key pressed
  , ctrl : Bool

  -- test
  , testInput : String
  }


type alias Window =
  { tree : TreeId

  -- , select : S.Set NodeId
  , selMain : Maybe NodeId
    -- ^ Main selected node (if any)
  , selAux : S.Set NodeId
    -- ^ Auxiliary selected nodes;
    -- invariant: selMain not in selAux

  -- , select : S.Set NodeId
  , pos : Position
  , drag : Maybe Drag
  }


type alias Dim =
  { width : Int
  , height : Int
  , heightProp : Int
  }


-- -- | Link between two trees.
-- type alias Link =
--   { from : (TreeId, NodeId)
--   , to : (TreeId, NodeId) }


-- Information about dragging.
type alias Drag =
    { start : Position
    , current : Position
    }


-- | Focus selector.
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


-- | Retrieve all selected nodes.
selAll : Window -> S.Set NodeId
selAll win =
  S.union win.selAux <|
    case win.selMain of
      Nothing -> S.empty
      Just x  -> S.singleton x


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
    alter win =
      { win
          | tree = switch win.tree model
          -- , select = S.empty
          , selMain = Nothing
          , selAux = S.empty
      }
    update foc = Lens.update foc alter model
  in
    case model.focus of
      Top -> update top
      Bot -> update bot


---------------------------------------------------
-- Select
---------------------------------------------------


-- We bypass the focus of the model since the node can be possibly selected
-- before the window it is in is even focused on!
selectNode : Focus -> NodeId -> Model -> Model
selectNode focus i model =
  let
    alter win =
      { win
          | selMain = Just i
--               if win.selMain == Just i
--               then Nothing
--               else Just i
          , selAux = S.empty
      }
    update lens = Lens.update lens alter model
  in
    case focus of
      Top -> update top
      Bot -> update bot


-- We bypass the focus of the model since the node can be possibly selected
-- before the window it is in is even focused on!
selectNodeAux : Focus -> NodeId -> Model -> Model
selectNodeAux focus i model =
  let
    alter win =
      if win.selMain == Just i
      then win
      else if S.member i win.selAux
      then {win | selAux = S.remove i win.selAux}
      else {win | selAux = S.insert i win.selAux}
    update lens = Lens.update lens alter model
  in
    case focus of
      Top -> update top
      Bot -> update bot


---------------------------------------------------
-- Labels
---------------------------------------------------


getLabel : NodeId -> Focus -> Model -> String
getLabel id focus model =
  let
    search (R.Node x ts) = if id == Lens.get nodeId x
      then Just (Lens.get nodeVal x)
      else searchF ts
    searchF ts = case ts of
      [] -> Nothing
      hd :: tl -> Util.mappend (search hd) (searchF tl)
    tree = getTree (selectWin focus model).tree model
  in
    Maybe.withDefault "?" <| search tree


setLabel : NodeId -> Focus -> String -> Model -> Model
setLabel id focus newLabel model =
  let
    update (R.Node x ts) = if id == Lens.get nodeId x
      -- then R.Node {x | nodeVal = newLabel} ts
      then R.Node (Lens.set nodeVal newLabel x) ts
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


-- | Process selected nodes in a given window.
procSel
  :  (S.Set NodeId -> R.Tree Node -> R.Tree Node)
  -> Focus -> Model -> Model
procSel f focus model =
  let
    win = selectWin focus model
    tree = getTree win.tree model
    newTree = f (selAll win) tree
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
deleteNode id tree =
  let
    update (R.Node x ts) =
      if id == Lens.get nodeId x && not (isLeaf x)
      then ts
      else [R.Node x (updateF ts)]
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd ++ updateF tl
  in
    case update tree of
      [hd] -> hd
--         if wellFormed hd
--         then hd
--         else tree
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
    rootId (R.Node x _) = Lens.get nodeId x
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
          <| L.map (\x -> Lens.get nodeId x)
          <| Util.catMaybes
          <| R.flatten tree
    newId1 = case findMaxMay of
       Nothing -> 1
       Just ix -> ix + 1
    update newId nodeMay =
      case nodeMay of
        Nothing -> (newId+1, Node {nodeId=newId, nodeVal=dummyVal})
        Just x  -> (newId, x)
  in
    Tuple.second <| R.mapAccum update newId1 tree


---------------------------------------------------
-- Add links
---------------------------------------------------


connect : Model -> Model
connect model = model |>
  case (model.focus, model.top.selMain, model.bot.selMain) of
    (Top, Just topNode, Just botNode) ->
      connectHelp {nodeFrom = botNode, nodeTo = topNode, focusTo = Top}
    (Bot, Just topNode, Just botNode) ->
      connectHelp {nodeFrom = topNode, nodeTo = botNode, focusTo = Bot}
    _ -> identity
    -- _ -> Debug.crash "ALALALAL"


type alias LinkInfo =
  { nodeFrom : NodeId
  , nodeTo : NodeId
  , focusTo : Focus }


connectHelp : LinkInfo -> Model -> Model
connectHelp {nodeFrom, nodeTo, focusTo} model =
  let
    focusFrom = case focusTo of
      Top -> Bot
      Bot -> Top
    treeFrom = (selectWin focusFrom model).tree
    treeTo = (selectWin focusTo model).tree
    link = ((treeFrom, nodeFrom), (treeTo, nodeTo))
    alter = case S.member link model.links of
      False -> S.insert link
      True  -> S.remove link
  in
    Lens.update links alter model


---------------------------------------------------
-- Attach subtree
---------------------------------------------------


-- | Perform attachement based on the selected node.
attachSel : Model -> Model
attachSel model =
  let
    focus = model.focus
    win = selectWin focus model
    fromMay = win.selMain
    toMay = case S.toList win.selAux of
      [to] -> Just to
      _    -> Nothing
    inTree = getTree win.tree model
  in
    updateSelect focus <|
      case (fromMay, toMay) of
        (Just from, Just to) ->
          case attach from to inTree of
            Just newTree ->
              Lens.update trees (D.insert win.tree newTree) model
            Nothing -> model
        _ -> model


-- | Copy a tree from a given place and paste it in another place in a given
-- tree.
attach
   : NodeId -- ^ From
  -> NodeId -- ^ To
  -> R.Tree Node -- ^ In
  -> Maybe (R.Tree Node)
attach from to tree =
  let
    p id x = Lens.get nodeId x == id
    putSubTree sub id = R.putSubTree sub (p id)
    getSubTree id = R.getSubTree (p id)
    delSubTree id = R.delSubTree (p id)
  in
    if isSubTree to from tree
    then Nothing
    else getSubTree from tree
      |> Maybe.andThen (\sub -> delSubTree from tree
      |> Maybe.map (\tree1 -> putSubTree sub to tree1)
      |> Maybe.andThen (Util.guard wellFormed)
      |> Maybe.map (sortTree to))
      -- |> Maybe.andThen (Util.guard wellFormed))


sortTree : NodeId -> R.Tree Node -> R.Tree Node
sortTree id =
  let
    leafPos x = case x of
      Leaf r -> r.leafPos
      _ -> Debug.crash "sortTree: should never happen"
    pred x = Lens.get nodeId x == id
  in
    R.sortTree leafPos pred


isSubTree
   : NodeId -- ^ x
  -> NodeId -- ^ y
  -> R.Tree Node -- ^ the underlying tree
  -> Bool   -- ^ is `x` in a subtree rooted in `y`?
isSubTree subId ofId tree =
  S.member ofId (ancestors subId tree)


-- | The set of ancestors (IDs) of a given node in a given tree.
ancestors : NodeId -> R.Tree Node -> S.Set NodeId
ancestors id tree =
  let
    go acc (R.Node x ts) =
      if Lens.get nodeId x == id
      then Just acc
      else
        let newAcc = S.insert (Lens.get nodeId x) acc
        in  L.foldl Util.mappend Nothing (L.map (go newAcc) ts)
  in
    Maybe.withDefault S.empty <| go S.empty tree


---------------------------------------------------
-- Utils
---------------------------------------------------


-- | Update the set of the selected nodes depending on the window in which the
-- tree was modified.
updateSelect : Focus -> Model -> Model
updateSelect foc model =
  let
    alter win =
      {win | selMain = Nothing, selAux = S.empty}
  in
    model |> case foc of
      Top -> Lens.update top alter
      Bot -> Lens.update bot alter


---------------------------------------------------
-- Lenses
---------------------------------------------------


top : Lens.Focus { record | top : a } a
top = Lens.create
  .top
  (\fn model -> {model | top = fn model.top})


bot : Lens.Focus { record | bot : a } a
bot = Lens.create
  .bot
  (\fn model -> {model | bot = fn model.bot})


trees : Lens.Focus { record | trees : a } a
trees = Lens.create
  .trees
  (\fn model -> {model | trees = fn model.trees})


winLens : Focus -> Lens.Focus { record | bot : a, top : a } a
winLens focus =
  case focus of
    Top -> top
    Bot -> bot


dim : Lens.Focus { record | dim : a } a
dim = Lens.create
  .dim
  (\fn model -> {model | dim = fn model.dim})


links : Lens.Focus { record | links : a } a
links = Lens.create
  .links
  (\fn model -> {model | links = fn model.links})

-- select : Lens.Focus { record | select : a } a
-- select = Lens.create
--   .select
--   (\fn model -> {model | select = fn model.select})


pos : Lens.Focus { record | pos : a } a
pos = Lens.create
  .pos
  (\fn model -> {model | pos = fn model.pos})


drag : Lens.Focus { record | drag : a } a
drag = Lens.create
  .drag
  (\fn model -> {model | drag = fn model.drag})


tree : Lens.Focus { record | tree : a } a
tree = Lens.create
  .tree
  (\fn model -> {model | tree = fn model.tree})


height : Lens.Focus { record | height : a } a
height = Lens.create
  .height
  (\fn model -> {model | height = fn model.height})


heightProp : Lens.Focus { record | heightProp : a } a
heightProp = Lens.create
  .heightProp
  (\fn model -> {model | heightProp = fn model.heightProp})


nodeId : Lens.Focus Node NodeId
nodeId =
  let
    get node = case node of
      Node r -> r.nodeId
      Leaf r -> r.nodeId
    update f node = case node of
      Node r -> Node {r | nodeId = f r.nodeId}
      Leaf r -> Leaf {r | nodeId = f r.nodeId}
  in
    Lens.create get update


nodeVal : Lens.Focus Node String
nodeVal =
  let
    get node = case node of
      Node r -> r.nodeVal
      Leaf r -> r.nodeVal
    update f node = case node of
      Node r -> Node {r | nodeVal = f r.nodeVal}
      Leaf r -> Leaf {r | nodeVal = f r.nodeVal}
  in
    Lens.create get update


---------------------------------------------------
-- Pseudo-lenses
---------------------------------------------------


-- | A pseudo-lens.
setTrees : TreeMap -> Model -> Model
setTrees treeDict model =
  let
    treeId = case D.toList treeDict of
      (id, tree) :: _ -> id
      _ -> Debug.crash "setTrees: empty tree dictionary"
  in
    {model | trees = treeDict}
      |> Lens.set (top => tree) treeId
      |> Lens.set (bot => tree) treeId
      |> updateSelect Top
      |> updateSelect Bot


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


fileDecoder : Decode.Decoder File
fileDecoder =
  Decode.map2 File
    (Decode.field "treeMap" treeMapDecoder)
    (Decode.field "linkSet" linkSetDecoder )


linkSetDecoder : Decode.Decoder (S.Set Link)
linkSetDecoder = Decode.map S.fromList <| Decode.list linkDecoder


linkDecoder : Decode.Decoder Link
linkDecoder =
  Decode.map2 (\from to -> (from, to))
    (Decode.field "from" addrDecoder)
    (Decode.field "to" addrDecoder)


addrDecoder : Decode.Decoder Addr
addrDecoder =
  Decode.map2 (\treeId nodeId -> (treeId, nodeId))
    (Decode.index 0 Decode.string)
    (Decode.index 1 Decode.int)


treeMapDecoder : Decode.Decoder TreeMap
treeMapDecoder = Decode.dict treeDecoder


treeDecoder : Decode.Decoder (R.Tree Node)
treeDecoder = R.treeDecoder nodeDecoder


nodeDecoder : Decode.Decoder Node
nodeDecoder = Decode.oneOf [internalDecoder, leafDecoder]


internalDecoder : Decode.Decoder Node
internalDecoder =
  Decode.map2 (\id val -> Node {nodeId=id, nodeVal=val})
    (Decode.field "nodeId" Decode.int)
    (Decode.field "nodeVal" Decode.string)


leafDecoder : Decode.Decoder Node
leafDecoder =
  Decode.map3 (\id val pos -> Leaf {nodeId=id, nodeVal=val, leafPos=pos})
    (Decode.field "leafId" Decode.int)
    (Decode.field "leafVal" Decode.string)
    (Decode.field "leafPos" Decode.int)


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeFile : File -> Encode.Value
encodeFile file =
  Encode.object
    [ ("tag", Encode.string "File")
    , ("treeMap", encodeTreeMap file.treeMap)
    , ("linkSet", encodeLinkSet file.linkSet)
    ]


encodeLinkSet : S.Set Link -> Encode.Value
encodeLinkSet =
  Encode.list << L.map encodeLink << S.toList


encodeLink : Link -> Encode.Value
encodeLink (from, to) =
  Encode.object
    [ ("tag", Encode.string "Link")
    , ("from", encodeAddr from)
    , ("to", encodeAddr to)
    ]


encodeAddr : Addr -> Encode.Value
encodeAddr (treeId, nodeId) = Encode.list
  [ Encode.string treeId
  , Encode.int nodeId ]


encodeTreeMap : TreeMap -> Encode.Value
encodeTreeMap =
  let encodePair (treeId, tree) = (treeId, encodeTree tree)
  in Encode.object << L.map encodePair << D.toList


encodeTree : R.Tree Node -> Encode.Value
encodeTree = R.encodeTree encodeNode


encodeNode : Node -> Encode.Value
encodeNode node = case node of
  Leaf r -> Encode.object
    [ ("tag", Encode.string "Leaf")
    , ("leafId", Encode.int r.nodeId)
    , ("leafVal", Encode.string r.nodeVal)
    , ("leafPos", Encode.int r.leafPos)
    ]
  Node r -> Encode.object
    [ ("tag", Encode.string "Node")
    , ("nodeId", Encode.int r.nodeId)
    , ("nodeVal", Encode.string r.nodeVal)
    ]
