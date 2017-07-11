module Edit.Model exposing
  (
  -- Data types:
    TreeMap, Sent, FileId, File, NodeId, TreeId
  , Node(..), NodeTyp(..), Link, Addr
  , isNode, isLeaf
  -- Model types:
  , Model, Dim, Window, SideWindow(..), Drag, Focus(..)
  -- Other:
  , selectWin, dragOn, getTree, selAll
  , getPosition, nextTree, prevTree, moveCursor, moveCursorTo
  , treeNum, treePos
  -- History:
  , freezeHist, undo, redo
  -- Selection:
  , updateSelection
  -- Logging:
  , log
  -- Nodes:
  , getNode, setNode, updateNode
  -- Labels:
  , getLabel, setLabel
  -- Event lenses:
  , eventClass, eventType, eventTense, eventAspect, eventPolarity, eventSubjMood
  , eventModality, eventComment
  -- Event modification:
  , setEventAttr -- , setEventClass, setEventType, setEventTense, setEventAspect
  -- Node selection:
  , selectNode, selectNodeAux
  -- Links
  , connect -- LinkInfo
  -- Tree modifications:
  , attachSel, deleteSel, deleteSelTree, addSel, swapSel
  -- Node annotation:
  , changeTypeSel
  -- Lenses:
  , top, bot, dim, winLens, drag, side, pos, height, widthProp, heightProp
  , nodeId, nodeVal, trees
  -- Pseudo-lenses:
  , setTrees
  -- Various:
  , setTree
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
import Edit.Anno as Anno


---------------------------------------------------
-- Data types
---------------------------------------------------


type alias FileId = String


type alias File =
  { treeMap : TreeMap
  , turns : List Turn
  , linkSet : S.Set Link }


-- | An original sentence.
type alias Sent = String


type alias TreeMap = D.Dict TreeId (Sent, R.Tree Node)


-- | A speech turn.
type alias Turn =
  { speaker : List String
  , trees : D.Dict TreeId (Maybe Int)
  }


-- | Link between two trees.
type alias Link = (Addr, Addr)
type alias Addr = (TreeId, NodeId)


-- | Tree identifier
type alias TreeId = Int
-- type alias TreeId = String


-- | Internal node identifier
type alias NodeId = Int


-- -- | Leaf identifier
-- type alias LeafId = Int


-- | Node in a syntactic tree is either an internal node or a leaf.
type Node
  = Node
    { nodeId : NodeId
    , nodeVal : String
    , nodeTyp : Maybe NodeTyp }
  | Leaf
    { nodeId : NodeId
    , nodeVal : String
      -- | The position of the leaf in the underlying sentence.
      -- The positions do not have to be consecutive.
    , leafPos : Int }


type NodeTyp
    = NodeEvent Anno.Event
    | NodeTimex


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

  -- the underlying map of trees
  , trees : TreeMap
  -- the list of turns (TODO: trees, turns, and links, could be grouped in a
  -- file)
  , turns : List Turn

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
  -- , testInput : String

  , messages : List String

  -- edit history
  , undoHist : List HistElem
  , redoHist : List HistElem

  -- last, incomplete element of the undo history
  , undoLast : List HistAtom
  }


type alias Window =
  { tree : TreeId

  -- | Main selected node (if any)
  , selMain : Maybe NodeId
  -- | Auxiliary selected nodes;
  -- invariant: selMain not in selAux
  , selAux : S.Set NodeId

  -- | Window's position shift
  , pos : Position

  -- | Window's drag
  , drag : Maybe Drag

  -- | Information about the side window.
  , side : SideWindow
  }


-- | Possible states of the side window.
type SideWindow
  = SideEdit
    -- ^ The main editing window
  | SideContext
    -- ^ Context window
  | SideLog
    -- ^ Messages


type alias Dim =
  { width : Int
  , height : Int
  , widthProp : Int
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
-- History
---------------------------------------------------


-- | An element of the editing history.
type alias HistElem = List HistAtom


-- | An atomic element of the editing history.
type HistAtom
  = TreeModif -- ^ Modified a tree
    { treeId : TreeId
      -- ^ To which tree ID does it refer
    , restoreTree : R.Tree Node
      -- ^ The tree to restore
    }
  | LinkModif
    { addLinkSet : S.Set Link
      -- ^ The links to add
    , delLinkSet : S.Set Link
      -- ^ The links to remove
    }


-- | Save the given atomic modification in the editing history.
saveModif : HistAtom -> Model -> Model
saveModif atom model =
  { model
      | undoLast = atom :: model.undoLast
      , redoHist = [] }


-- | Freeze the current (last) sequence of history modifications.
--
-- The idea is that everything that is in `undoLast` will be transfered to
-- `undoHist` as single, atomic element of the editing history. Put differently,
-- the current atomic modifications are put in a transaction.
freezeHist : Model -> Model
freezeHist model =
  let
    histSize = 250 -- should be in Config, but Config relies on the model...
    newHist = L.take histSize (model.undoLast :: model.undoHist)
  in
  case model.undoLast of
    [] -> model
    _  -> { model
              | undoHist = newHist
              , undoLast = [] }


-- | Apply a given history atomic element.
applyAtom : HistAtom -> Model -> (Model, HistAtom)
applyAtom histAtom model =
  case histAtom of
    TreeModif r ->
      let
        oldTree = getTree r.treeId model
        tmpModel = updateTree_ r.treeId (\_ -> r.restoreTree) model
        newModel = focusOnTree r.treeId tmpModel
        newAtom = TreeModif {r | restoreTree = oldTree}
      in
        (newModel, newAtom)
    LinkModif r ->
      let
        newLinks = S.union r.addLinkSet <| S.diff model.links r.delLinkSet
        newModel = {model | links = newLinks}
        newAtom = LinkModif {addLinkSet = r.delLinkSet, delLinkSet = r.addLinkSet}
      in
        (newModel, newAtom)


-- | Focus on the given tree if needed.
focusOnTree : TreeId -> Model -> Model
focusOnTree treeId model =
    if model.top.tree == treeId || model.bot.tree == treeId
    then model
    else moveCursorTo model.focus treeId model


-- | Apply a given history element.
applyElem : HistElem -> Model -> (Model, HistElem)
applyElem elem =
  let
    apply xs model histAcc = case xs of
      [] -> (model, histAcc)
      hd :: tl ->
        let (newModel, newAtom) = applyAtom hd model
        in  apply tl newModel (newAtom :: histAcc)
  in
    -- \model -> Debug.log (toString elem) <| apply elem model []
    \model -> apply elem model []


-- | Perform undo.
undo : Model -> Model
undo model =
  case model.undoHist of
    [] -> model
    histElem :: histRest ->
      let (newModel, newElem) = applyElem histElem model
      in  { newModel
            | undoHist = histRest
            , redoHist = newElem :: newModel.redoHist }


-- | Perform redo.
redo : Model -> Model
redo model =
  case model.redoHist of
    [] -> model
    histElem :: histRest ->
      let (newModel, newElem) = applyElem histElem model
      in  { newModel
            | redoHist = histRest
            , undoHist = newElem :: newModel.undoHist }


---------------------------------------------------
-- Logging
---------------------------------------------------


-- | Log a message.
log : String -> Model -> Model
log msg model =
  let
    logSize = 100 -- should be in Config, but Config relies on the model...
    newMessages = L.take logSize (msg :: model.messages)
  in
    {model | messages = newMessages}


---------------------------------------------------
---------------------------------------------------
-- Primitive modification operations
---------------------------------------------------
---------------------------------------------------


---------------------------------------------------
-- Tree-wise
---------------------------------------------------


-- -- | Set a tree under a given ID.
-- updateTree : TreeId -> (R.Tree Node -> R.Tree Node) -> Model -> Model
-- updateTree treeId update model =
--   let
--     alter v = case v of
--       Nothing -> Debug.crash "Model.updateTree: no tree with the given ID"
--       Just (sent, tree) -> Just (sent, update tree)
--     oldTree = case D.get treeId model.trees of
--       Nothing -> Debug.crash "Model.updateTree: no tree with the given ID"
--       Just (sent_, tree) -> tree
--     newTrees = D.update treeId alter model.trees
--   in
--     {model | trees = newTrees}
--        |> saveModif (TreeModif {treeId = treeId, restoreTree = oldTree})


-- | Set a tree under a given ID.
updateTree : TreeId -> (R.Tree Node -> R.Tree Node) -> Model -> Model
updateTree treeId update model =

  let

    -- calculate the new tree and update the model
    oldTree = getTree treeId model
    newModel = updateTree_ treeId update model
    newTree = getTree treeId newModel

    -- we also calculate the set of deleted nodes
    delNodes = S.diff (nodesIn oldTree) (nodesIn newTree)

    -- and delete the corresponding links, if needed
    isToDelete (from, to) =
      let toDel (trId, ndId) = trId == treeId && S.member ndId delNodes
      in  toDel from || toDel to
    delLinks = L.filter isToDelete <| S.toList model.links
    linkModif = deleteLinks (S.fromList delLinks)

  in

     newModel
       |> linkModif
       |> saveModif (TreeModif {treeId = treeId, restoreTree = oldTree})
--        |> updateSelection Top <- performed always after the update
--        |> updateSelection Bot


-- | An internal version of `updateTree` which does not update the history.
updateTree_ : TreeId -> (R.Tree Node -> R.Tree Node) -> Model -> Model
updateTree_ treeId update model =
  let
    alter v = case v of
      Nothing -> Debug.crash "Model.updateTree_: no tree with the given ID"
      Just (sent, tree) -> Just (sent, update tree)
    newTrees = D.update treeId alter model.trees
    newModel = {model | trees = newTrees}
  in
    newModel


-- | Return the set of node IDs in the given tree.
nodesIn : R.Tree Node -> S.Set NodeId
nodesIn = S.fromList << L.map (Lens.get nodeId) << R.flatten


---------------------------------------------------
-- Link-wise
---------------------------------------------------


-- | Delete the set of links.
deleteLinks : S.Set Link -> Model -> Model
deleteLinks delLinks =
  if S.isEmpty delLinks
  then identity
  else Lens.update links (\s -> S.diff s delLinks)
    >> saveModif (LinkModif {addLinkSet = delLinks, delLinkSet = S.empty})


-- | Add links.
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
    (alter, modif) = case S.member link model.links of
      False ->
        ( S.insert link
        , LinkModif {addLinkSet = S.empty, delLinkSet = S.singleton link} )
      True  ->
        ( S.remove link
        , LinkModif {delLinkSet = S.empty, addLinkSet = S.singleton link} )
  in
    Lens.update links alter model |> saveModif modif


---------------------------------------------------
-- Selection-wise?
---------------------------------------------------


-- | Update the selections.
updateSelection : Model -> Model
updateSelection =
  updateSelection_ Top >>
  updateSelection_ Bot


-- | Update the selection in the given window.
updateSelection_ : Focus -> Model -> Model
updateSelection_ focus model =
  let
    wlen = winLens focus
    win = Lens.get wlen model -- selectWin focus model
    tree = getTree win.tree model
    newID id = case getNode_ id tree of
      Nothing -> Nothing
      Just _  -> Just id
    newWin =
      { win
        | selMain = Maybe.andThen newID win.selMain
        , selAux
             = S.fromList
            <| Util.catMaybes
            <| L.map newID
            <| S.toList win.selAux
      }
  in
    Lens.set wlen newWin model


---------------------------------------------------
---------------------------------------------------
-- Misc
---------------------------------------------------
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
getTree treeId model =
  case D.get treeId model.trees of
    Nothing -> Debug.crash "Model.getTree: no tree with the given ID"
    Just (_, t) -> t


-- -- | Set a tree under a given ID.
-- setTree : TreeId -> R.Tree Node -> Model -> Model
-- setTree treeId newTree model =
--   let
--     alter v = case v of
--       Nothing -> Debug.crash "Model.setTree: no tree with the given ID"
--       Just (sent, _) -> Just (sent, newTree
--     newTrees = D.update treeId alter model.trees
--   in
--     {model | tree = newTrees}


-- | Retrieve all selected nodes.
selAll : Window -> S.Set NodeId
selAll win =
  S.union win.selAux <|
    case win.selMain of
      Nothing -> S.empty
      Just x  -> S.singleton x


-- | Change a tree in focus, provided that it has the appropriate
-- file and tree IDs.
setTree : FileId -> TreeId -> R.Tree Node -> Model -> Model
setTree fileId treeId tree model =
  let
    win = selectWin model.focus model
    treeIdSel = win.tree
    fileIdSel = model.fileId
  in
    if fileId == fileIdSel && treeId == treeIdSel
    then updateTree win.tree (\_ -> tree) model
    else model


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
    win = selectWin model.focus model
    switch = if next then nextTree else prevTree
    treeId = switch win.tree model
  in
    moveCursorTo model.focus treeId model
-- moveCursor next model =
--   let
--     switch = if next then nextTree else prevTree
--     alter win =
--       { win
--           | tree = switch win.tree model
--           -- , select = S.empty
--           , selMain = Nothing
--           , selAux = S.empty
--       }
--     update foc = Lens.update foc alter model
--   in
--     case model.focus of
--       Top -> update top
--       Bot -> update bot


-- | Similar to `moveCursor`, but the tree ID as well as the focus are directly
-- specified.
moveCursorTo : Focus -> TreeId -> Model -> Model
moveCursorTo focus treeId model =
  let
    alter win =
      { win
          | tree = treeId
          , selMain = Nothing
          , selAux = S.empty
      }
    update foc = Lens.update foc alter model
  in
    case focus of
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
-- Node modification
---------------------------------------------------


getNode : NodeId -> Focus -> Model -> Node
getNode id focus model =
  let
    tree = getTree (selectWin focus model).tree model
  in
    case getNode_ id tree of
      Nothing -> Debug.crash "Model.getNode: unknown node ID"
      Just x  -> x


getNode_ : NodeId -> R.Tree Node -> Maybe Node
getNode_ id tree =
  let
    search (R.Node x ts) = if id == Lens.get nodeId x
      then Just x
      else searchF ts
    searchF ts = case ts of
      [] -> Nothing
      hd :: tl -> Util.mappend (search hd) (searchF tl)
  in
    search tree


updateNode : NodeId -> Focus -> (Node -> Node) -> Model -> Model
updateNode id focus updNode model =
  let
    update (R.Node x ts) = if id == Lens.get nodeId x
      then R.Node (updNode x) ts
      else R.Node x (updateF ts)
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd :: updateF tl
    win = selectWin focus model
  in
    updateTree win.tree update model


setNode : NodeId -> Focus -> Node -> Model -> Model
setNode id focus newNode = updateNode id focus (\_ -> newNode)


---------------------------------------------------
-- Labels
---------------------------------------------------


getLabel : NodeId -> Focus -> Model -> String
getLabel id focus model =
    Lens.get nodeVal <| getNode id focus model


setLabel : NodeId -> Focus -> String -> Model -> Model
setLabel id focus newLabel model =
    let update = Lens.set nodeVal newLabel
    in  updateNode id focus update model


---------------------------------------------------
-- Event modification
---------------------------------------------------


setEventAttr : (Lens.Focus Anno.Event a) -> NodeId -> Focus -> a -> Model -> Model
setEventAttr attLens id focus newVal model =
    let lens = nodeTyp => maybeLens => nodeEvent => attLens
        update = Lens.set lens newVal
    in  updateNode id focus update model


-- setEventClass : NodeId -> Focus -> Anno.EventClass -> Model -> Model
-- setEventClass = setEventAttr eventClass
--
-- setEventType : NodeId -> Focus -> Anno.EventType -> Model -> Model
-- setEventType = setEventAttr eventType
--
-- setEventTense : NodeId -> Focus -> Maybe Anno.EventTense -> Model -> Model
-- setEventTense = setEventAttr eventTense
--
-- setEventAspect : NodeId -> Focus -> Maybe Anno.EventAspect -> Model -> Model
-- setEventAspect = setEventAttr eventAspect


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
  in
    updateTree win.tree (\_ -> newTree) model


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
-- Delete Tree
---------------------------------------------------


-- | Delete the selected nodes in a given window, together with the
-- corresponding subtrees.
deleteSelTree : Focus -> Model -> Model
deleteSelTree =
  let f ids t = L.foldl deleteTree t (S.toList ids)
  in  procSel f


-- | Delete a given node (provided that it is not a root) together with the
-- corresponding subtree.
deleteTree : NodeId -> R.Tree Node -> R.Tree Node
deleteTree id tree =
  let
    update (R.Node x ts) =
      if id == Lens.get nodeId x -- && not (isLeaf x)
      then []
      else [R.Node x (updateF ts)]
    updateF ts = case ts of
      [] -> []
      hd :: tl -> update hd ++ updateF tl
  in
    case update tree of
      [hd] ->
        if wellFormed hd
        then hd
        else tree
      _ -> tree -- A situation which can occur if you delete a root


---------------------------------------------------
-- Add
---------------------------------------------------


-- | Add selected nodes in a given window.
addSel : Focus -> Model -> Model
addSel = procSel (\ids -> addNode ids >> addRoot ids)


-- | Add parent to a root (if in the set of selected nodes).
addRoot : S.Set NodeId -> R.Tree Node -> R.Tree Node
addRoot ids t =
  let
    rootId (R.Node x _) = Lens.get nodeId x
  in
    if S.member (rootId t) ids
    then identify "?" <| R.Node Nothing [R.map Just t]
    else t


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
        Nothing -> (newId+1, Node {nodeId=newId, nodeVal=dummyVal, nodeTyp=Nothing})
        Just x  -> (newId, x)
  in
    Tuple.second <| R.mapAccum update newId1 tree


---------------------------------------------------
-- Change the type of the selected node
---------------------------------------------------


-- | Change the type of the main selected nodes in a given window.
changeTypeSel : Focus -> Model -> Model
changeTypeSel focus model =
  let
    win = selectWin focus model
    tree = getTree win.tree model
    -- newTree id = changeType id tree
  in
    case win.selMain of
      Nothing -> model
      Just id -> updateTree win.tree (\_ -> changeType id tree) model


-- | Delete a given node, provided that it is not a root.
changeType : NodeId -> R.Tree Node -> R.Tree Node
changeType id =
  let
    update x =
      if id == Lens.get nodeId x && not (isLeaf x)
      then shift x
      else x
    shift node = case node of
      Leaf r -> node
      Node r -> Node <| {r | nodeTyp = shiftTyp r.nodeTyp}
    shiftTyp x = case x of
      Nothing -> Just <| NodeEvent Anno.eventDefault
      Just (NodeEvent _) -> Just NodeTimex
      Just NodeTimex -> Nothing
  in
    R.map update


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
    case (fromMay, toMay) of
      (Just from, Just to) ->
        case attach from to inTree of
          Just newTree ->
            -- Lens.update trees (D.insert win.tree newTree) model
            updateTree win.tree (\_ -> newTree) model
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
-- Shift subtree
---------------------------------------------------


-- | Perform swap based on the selected node.
swapSel : Bool -> Model -> Model
swapSel left model =
  let
    focus = model.focus
    win = selectWin focus model
    nodeMay = win.selMain
    inTree = getTree win.tree model
    -- left = not model.ctrl
  in
    nodeMay
      |> Maybe.andThen (\nodeId -> swap left nodeId inTree
      |> Maybe.map (\newTree -> updateTree win.tree (\_ -> newTree) model))
      |> Maybe.withDefault model
--     case nodeMay of
--       Just nodeId ->
--         case swap left nodeId inTree of
--           Just newTree -> updateTree win.tree (\_ -> newTree) model
--           Nothing -> model
--       _ -> model


-- | Shift the tree attached at the given onde right or left.
swap
   : Bool -- ^ Right or left?
  -> NodeId -- ^ Which node?
  -> R.Tree Node -- ^ In which tree?
  -> Maybe (R.Tree Node)
swap left id tree =
  let
    p x = Lens.get nodeId x == id
  in
    R.swapSubTree left p tree
      |> Util.guard wellFormed


---------------------------------------------------
-- Utils
---------------------------------------------------


-- -- | Update the set of the selected nodes depending on the window in which the
-- -- tree was modified.
-- updateSelect : Focus -> Model -> Model
-- updateSelect foc model =
--   let
--     alter win =
--       {win | selMain = Nothing, selAux = S.empty}
--   in
--     model |> case foc of
--       Top -> Lens.update top alter
--       Bot -> Lens.update bot alter


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


side : Lens.Focus { record | side : a } a
side = Lens.create
  .side
  (\fn model -> {model | side = fn model.side})


tree : Lens.Focus { record | tree : a } a
tree = Lens.create
  .tree
  (\fn model -> {model | tree = fn model.tree})


height : Lens.Focus { record | height : a } a
height = Lens.create
  .height
  (\fn model -> {model | height = fn model.height})


widthProp : Lens.Focus { record | widthProp : a } a
widthProp = Lens.create
  .widthProp
  (\fn model -> {model | widthProp = fn model.widthProp})


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


nodeTyp : Lens.Focus Node (Maybe NodeTyp)
nodeTyp =
  let
    getErr = "nodeTyp.lens: cannot get the nodeTyp"
    get node = case node of
      Node r -> r.nodeTyp
      Leaf r -> Debug.crash getErr
    update f node = case node of
      Node r -> Node {r | nodeTyp = f r.nodeTyp}
      Leaf r -> Leaf r
  in
    Lens.create get update


nodeEvent : Lens.Focus NodeTyp Anno.Event
nodeEvent =
  let
    getErr = "nodeEvent.lens: cannot get"
    get typ = case typ of
      NodeEvent event -> event
      _ -> Debug.crash getErr
    update f typ = case typ of
      NodeEvent event -> NodeEvent (f event)
      _ -> typ
  in
    Lens.create get update


----------------------------
-- Event-related lenses
----------------------------


eventClass : Lens.Focus Anno.Event Anno.EventClass
eventClass =
  let
    get (Anno.Event r) = r.evClass
    update f (Anno.Event r) = Anno.Event {r | evClass = f r.evClass}
  in
    Lens.create get update


eventType : Lens.Focus Anno.Event Anno.EventType
eventType =
  let
    get (Anno.Event r) = r.evType
    update f (Anno.Event r) = Anno.Event {r | evType = f r.evType}
  in
    Lens.create get update


eventTense : Lens.Focus Anno.Event (Maybe Anno.EventTense)
eventTense =
  let
    get (Anno.Event r) = r.evTense
    update f (Anno.Event r) = Anno.Event {r | evTense = f r.evTense}
  in
    Lens.create get update


eventAspect : Lens.Focus Anno.Event (Maybe Anno.EventAspect)
eventAspect =
  let
    get (Anno.Event r) = r.evAspect
    update f (Anno.Event r) = Anno.Event {r | evAspect = f r.evAspect}
  in
    Lens.create get update


eventPolarity : Lens.Focus Anno.Event Anno.EventPolarity
eventPolarity =
  let
    get (Anno.Event r) = r.evPolarity
    update f (Anno.Event r) = Anno.Event {r | evPolarity = f r.evPolarity}
  in
    Lens.create get update


eventSubjMood : Lens.Focus Anno.Event Bool
eventSubjMood =
  let
    get (Anno.Event r) = r.evSubjMood
    update f (Anno.Event r) = Anno.Event {r | evSubjMood = f r.evSubjMood}
  in
    Lens.create get update


eventModality : Lens.Focus Anno.Event (Maybe Anno.EventModality)
eventModality =
  let
    get (Anno.Event r) = r.evModality
    update f (Anno.Event r) = Anno.Event {r | evModality = f r.evModality}
  in
    Lens.create get update


eventComment : Lens.Focus Anno.Event String
eventComment =
  let
    get (Anno.Event r) = r.evComment
    update f (Anno.Event r) = Anno.Event {r | evComment = f r.evComment}
  in
    Lens.create get update


----------------------------
-- Utility lenses
----------------------------


maybeLens : Lens.Focus (Maybe a) a
maybeLens =
  let
    getErr = "maybeLens: got Nothing"
    get may = case may of
      Nothing -> Debug.crash getErr
      Just x -> x
    update f may = case may of
      Nothing -> Nothing
      Just x -> Just (f x)
  in
    Lens.create get update


---------------------------------------------------
-- Pseudo-lenses
---------------------------------------------------


-- | Change the treeMap of the model.
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
--       |> updateSelect Top
--       |> updateSelect Bot


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


fileDecoder : Decode.Decoder File
fileDecoder =
  Decode.map3 File
    (Decode.field "treeMap" treeMapDecoder)
    (Decode.field "turns" (Decode.list turnDecoder))
    (Decode.field "linkSet" linkSetDecoder )


turnDecoder : Decode.Decoder Turn
turnDecoder =
  let
    speakerDecoder = Decode.list Decode.string
    treesDecoder = Decode.map (mapKeys toInt) <| Decode.dict (Decode.nullable Decode.int)
  in
    Decode.map2 Turn
      (Decode.field "speaker" speakerDecoder)
      (Decode.field "trees" treesDecoder)


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
    -- (Decode.index 0 Decode.string)
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.int)


treeMapDecoder : Decode.Decoder TreeMap
treeMapDecoder = Decode.map (mapKeys toInt) <| Decode.dict <|
  Decode.map2 (\sent tree -> (sent, tree))
    (Decode.index 0 Decode.string)
    (Decode.index 1 treeDecoder)


treeDecoder : Decode.Decoder (R.Tree Node)
treeDecoder = R.treeDecoder nodeDecoder


nodeDecoder : Decode.Decoder Node
nodeDecoder = Decode.oneOf [internalDecoder, leafDecoder]


internalDecoder : Decode.Decoder Node
internalDecoder =
  Decode.map3 (\id val typ -> Node {nodeId=id, nodeVal=val, nodeTyp=typ})
    (Decode.field "nodeId" Decode.int)
    (Decode.field "nodeVal" Decode.string)
    (Decode.field "nodeTyp" (Decode.nullable nodeTypDecoder))


leafDecoder : Decode.Decoder Node
leafDecoder =
  Decode.map3 (\id val pos -> Leaf {nodeId=id, nodeVal=val, leafPos=pos})
    (Decode.field "leafId" Decode.int)
    (Decode.field "leafVal" Decode.string)
    (Decode.field "leafPos" Decode.int)


nodeTypDecoder : Decode.Decoder NodeTyp
nodeTypDecoder = Decode.oneOf [nodeEventDecoder, nodeTimexDecoder]


nodeEventDecoder : Decode.Decoder NodeTyp
nodeEventDecoder =
  Decode.map (\ev -> NodeEvent ev)
    (Decode.field "contents" Anno.eventDecoder)


nodeTimexDecoder : Decode.Decoder NodeTyp
nodeTimexDecoder =
  let
    verifyTag x = case x of
      "NodeTimex" -> Decode.succeed NodeTimex
      _ -> Decode.fail "not a NodeTimex"
  in
    Decode.field "tag" Decode.string |> Decode.andThen verifyTag


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeFile : File -> Encode.Value
encodeFile file =
  Encode.object
    [ ("tag", Encode.string "File")
    , ("treeMap", encodeTreeMap file.treeMap)
    , ("turns", Encode.list (L.map encodeTurn file.turns))
    , ("linkSet", encodeLinkSet file.linkSet)
    ]


encodeTurn : Turn -> Encode.Value
encodeTurn turn =
  let
    -- speakerDecoder = Decode.list Decode.string
    -- treesDecoder = Decide.dict (Decode.nullable Decode.int)
    encodeSpeaker = Encode.list << L.map Encode.string
    encodePair (treeId, mayWho) =
      (toString treeId, Util.encodeMaybe Encode.int mayWho)
    encodeTrees = Encode.object << L.map encodePair << D.toList
  in
    Encode.object
      [ ("tag", Encode.string "Turn")
      , ("speaker", encodeSpeaker turn.speaker)
      , ("trees", encodeTrees turn.trees)
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
  -- [ Encode.string treeId
  [ Encode.int treeId
  , Encode.int nodeId ]


encodeTreeMap : TreeMap -> Encode.Value
encodeTreeMap =
  let
    encodeSentTree (sent, tree) = Encode.list
      [ encodeSent sent
      , encodeTree tree ]
    encodePair (treeId, sentTree) =
      (toString treeId, encodeSentTree sentTree)
  in
    Encode.object << L.map encodePair << D.toList


encodeSent : Sent -> Encode.Value
encodeSent = Encode.string


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
    , ("nodeTyp", Util.encodeMaybe encodeNodeTyp r.nodeTyp)
    ]


encodeNodeTyp : NodeTyp -> Encode.Value
encodeNodeTyp nodeTyp = case nodeTyp of
  NodeEvent ev -> Encode.object
    [ ("tag", Encode.string "NodeEvent")
    , ("contents", Anno.encodeEvent ev) ]
  NodeTimex -> Encode.object
    [ ("tag", Encode.string "NodeTimex") ]


---------------------------------------------------
-- Utils
---------------------------------------------------


toInt : String -> Int
toInt x = String.toInt x |> Result.toMaybe |> Maybe.withDefault 0


mapKeys
    : (comparable -> comparable2)
    -> D.Dict comparable c
    -> D.Dict comparable2 c
mapKeys f d =
  let first f (x, y) = (f x, y)
  in  D.fromList <| L.map (first f) <| D.toList <| d
