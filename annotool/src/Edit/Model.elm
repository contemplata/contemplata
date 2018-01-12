module Edit.Model exposing
  (
  -- Data types:
    TreeMap, Sent, Token, File, Turn
  , Node(..), NodeAnnoTyp
  , InternalNode, LeafNode
  -- , LinkData
  , isNode, isLeaf
  , concatToks, sentToString, emptyToken
  -- Model types:
  , Model, Dim, Window, SideWindow(..), Drag
  , Workspace
  -- Other:
  , selectWin, dragOn, getTree, getTreeMay, getReprId, selAll
  , getPosition, nextTree, nextTree_, prevTree, moveCursor, moveCursorTo
  , treeNum, treePos
  -- Sentence:
  , getToken, getTokenMay, getSent, getSubSent
  -- History:
  , freezeHist, undo, redo
  -- Selection:
  , updateSelection, setSelection
  -- Logging:
  , log
  -- Nodes:
  , getNode, setNode, updateNode
  , getNodeTyp, NodeTyp(..)
  -- TODO (think of the name):
  , concatWords, dumify, isDummyTree
  , splitTree, join, getPart
  , restoreToken, visiblePositions
  , syncForestWithSent, syncForestWithSentPos
  -- Labels:
  , getLabel, setLabel
  -- Comments:
  , getComment, setComment

--   -- Event lenses:
--   , eventClass, eventType, eventTime, eventAspect, eventPolarity, eventMood
--   , eventModality -- eventComment
--   , eventInquisit, eventCardinality, eventMod
--   , eventPred

  -- Entity modification:
  , setEntityType
  , setEntityAttr
  , setEntityAnchor

  -- -- Event modification:
  -- , setEventAttr
  -- -- Signal modification:
  -- , setSignalAttr
  -- -- Timex modification:
  -- , setTimexAttr
  -- , setTimexType, setTimexAnchor, setTimexBeginPoint, setTimexEndPoint
  -- , remTimexAnchor, remTimexBeginPoint, remTimexEndPoint

--   -- Signal lenses:
--   , signalType

--   -- Timex lenses:
--   , timexCalendar, timexType, timexPred, timexFunctionInDocument
--   , timexTemporalFunction, timexLingValue, timexValue, timexMod
--   , timexQuant, timexFreq


  -- Node selection:
  , selectNode, selectNodeAux
  -- Links
  , connect -- LinkInfo
  -- Tree modifications:
  , attachSel, deleteSel, deleteSelTree, addSel, swapSel
  -- Node annotation:
  -- , mkEventSel, mkSignalSel, mkTimexSel
  , mkEntitySel
  -- Popup-related
  , setFilesPopup, changeSplit, performSplit
  -- -- , changeTypeSel
  -- Lenses:
  , top, bot, dim, drag, side, pos, height, widthProp, heightProp
  , nodeId, nodeVal, treeMap, sentMap, partMap, reprMap, trees, fileLens
  , workspaceLens, windowLens, fileId, selMain, linkMap
  -- Pseudo-lenses:
  -- , setTrees
  , getFileId
  -- Various:
  , setTreeCheck, setForestCheck, setSentCheck, getWords, subTreeAt, nodesIn
  , swapFile, swapWorkspaces, moveToFirst, moveToTree, moveToFile
  , swapFiles
  -- JSON decoding:
  , treeMapDecoder, fileDecoder, treeDecoder, sentDecoder, nodeDecoder
  -- JSON encoding:
  , encodeFile, encodeToken
  )


import Mouse exposing (Position)

import Set as S
import Dict as D
import List as L
import String as Str
import Focus exposing ((=>))
import Focus as Lens
import Maybe as Maybe
import Either exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
-- import Json.Decode.Pipeline as DePipe

import Util as Util
import Config as Cfg
import Rose as R
import Edit.Core exposing (..)
import Edit.Config as AnnoCfg
import Edit.Anno as Anno
import Edit.Anno.Core as Anno
import Edit.Popup as Popup


---------------------------------------------------
-- Data types
---------------------------------------------------


type alias File =
  { treeMap : TreeMap
  , sentMap : D.Dict TreeIdBare Sent
  -- , partMap : D.Dict TreeIdBare (S.Set TreeIdBare)
  , partMap : D.Dict PartId (S.Set TreeIdBare)
  , reprMap : D.Dict TreeIdBare TreeIdBare
    -- ^ Direct representatives
  , turns : List Turn
  , linkMap : D.Dict Link Anno.Entity }


-- | An original sentence: a list of tokens
type alias Sent = List Token


type alias Token =
    { orth : String
      -- ^ NOTE: We assume that `orth` is trimmed (no whitespaces on any side).
    , afterSpace : Bool
      -- ^ Is the token after space or not?
      -- NOTE: `True` for leading tokens, for convenience (consider
    -- sentence concatenation)
    }


emptyToken : Token
emptyToken = {orth = "", afterSpace = True}


mergeToks : Token -> Token -> Token
mergeToks x y =
    let
        ySpace =
            if y.afterSpace
            then " "
            else ""
    in
        { orth = Str.concat [x.orth, ySpace, y.orth]
        , afterSpace = x.afterSpace }


concatToks : List Token -> Token
concatToks =
    let strip tok = {tok | orth = Str.trim tok.orth}
    in  strip << L.foldr mergeToks emptyToken


sentToString : Sent -> String
sentToString = .orth << concatToks
--     =  String.trim
--     << String.concat
--     << L.map (\tok -> (if tok.afterSpace then " " else "") ++ tok.orth)


type alias TreeMap = D.Dict PartId (R.Tree Node)


-- | A speech turn.
type alias Turn =
  { speaker : List String
  , trees : D.Dict TreeIdBare (Maybe Int)
  }


-- -- | Leaf identifier
-- type alias LeafId = Int


type alias InternalNode =
    { nodeId : NodeId
    , nodeVal : String
    , nodeTyp : Maybe NodeAnnoTyp
    , nodeComment : String }


type alias LeafNode =
    { nodeId : NodeId
    , nodeVal : String
      -- ^ Orth value, which is not necessarily equal to the orth values of the
      -- corresponding tokens.
    , leafPos : Int
      -- ^ The position of the leaf in the underlying sentence. The positions
      -- are not guaranteed to be consecutive (some tokens are not taken into
      -- account when parsing). In addition, in non-projective trees, the
      -- positions are not guaranteed to come in an increasing order.
    , nodeComment : String }


-- | Node in a syntactic tree is either an internal node or a leaf.
type Node
  = Node InternalNode
  | Leaf LeafNode


type alias NodeAnnoTyp = Anno.Entity
-- type NodeAnnoTyp
--     = NodeEvent Anno.Event
--     | NodeSignal Anno.Signal
--     | NodeTimex Anno.Timex


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


-- ---------------------------------------------------
-- -- LinkData
-- ---------------------------------------------------
--
--
-- -- | Data related to a link.
-- type alias LinkData =
--   { signalAddr : Maybe Addr
--     -- ^ Address of the corresponding signal, if any
--   }
--
--
-- -- | The default link data value.
-- linkDataDefault : LinkData
-- linkDataDefault =
--   { signalAddr = Nothing
--   }
--
--
-- -- | Switch the signal:
-- --
-- -- * If the same signal is already assigned to the link, switch it off
-- -- * Otherwise, add the new signal to the link (even if another signal
-- --   was previously set)
-- switchSignal : Addr -> LinkData -> LinkData
-- switchSignal signal r =
--   if Just signal == r.signalAddr
--   then {r | signalAddr = Nothing}
--   else {r | signalAddr = Just signal}


---------------------------------------------------
-- Model-related types
---------------------------------------------------


-- | Information about the annotated file. It includes the file itself, but also
-- information about the positioning and the selected tree and nodes.
type alias FileInfo =
    { file : File
    -- ^ The annotated file
    , top  : Window
    -- ^ Its view in the top window
    , bot  : Window
    -- ^ Its view in the bottom window

    -- Editing history
    , undoHist : List HistElem
    , redoHist : List HistElem

    -- Last, incomplete element of the undo history
    , undoLast : List HistAtom
    }


-- | Information about the top/bottom workspace.
type alias Workspace =
    { fileId : FileId    -- ^ The file in view in the top window
    , drag : Maybe Drag  -- ^ Workspace's drag
    , side : SideWindow  -- ^ Which side window is selected
    }


type alias Model =
  { fileList : List (FileId, FileInfo)
  -- ^ The list of annotated files, together with their IDs. The file IDs are
  -- guaranteed to be distinct (we don't use a map because `FileId` cannot be a
  -- key in Elm, only its encoded version, but the latter would be
  -- inconvenient).

  , top : Workspace
  , bot : Workspace

  -- which window is the focus on
  , focus : Focus

  -- , dragOn : Maybe (Focus, Drag)
  -- , dragOn : Maybe Focus

  -- | Selected link (if any)
  , selLink : Maybe Link

  -- window dimensions
  , dim : Dim

  -- is CTRL key pressed
  , ctrl : Bool

  -- test
  -- , testInput : String

  , messages : List String

  -- scripting window
  , command : Maybe String

  -- pop-up window
  , popup : Maybe Popup.Popup

  -- the external configuration
  , config : Cfg.Config

  -- the annotation configuration
  , annoConfig : AnnoCfg.Config

  -- which annotation level is in focus
  , annoLevel : AnnoLevel
  }


type alias Window =
  { tree : TreeId
  -- ^ NOTE: it is not guaranteed to be a tree representative (cf. file partitions)
  -- ^ NOTE: this ID refers to the `file` in the corresponding `FileInfo`

  -- | Main selected node (if any)
  , selMain : Maybe NodeId
  -- | Auxiliary selected nodes;
  -- invariant: selMain not in selAux
  , selAux : S.Set NodeId

  -- | Window's position shift
  , pos : Position
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


---------------------------------------------------
-- History
---------------------------------------------------


-- | An element of the editing history.
type alias HistElem = List HistAtom


-- | An atomic element of the editing history.
type HistAtom
  = TreeModif -- ^ Modified a tree
    { treeId : PartId
      -- ^ To which tree ID does it refer
    , restoreTree : Maybe (R.Tree Node)
      -- ^ The tree to restore (or remove, if `Nothing`)
    }
  | LinkModif
    { addLinkSet : D.Dict Link Anno.Entity
      -- ^ The links to add
    , delLinkSet : D.Dict Link Anno.Entity
      -- ^ The links to remove
    }
  | PartModif -- ^ Modified a partition
    { treeId : PartId
      -- ^ To which partition ID does it refer
    , restorePart : Maybe (S.Set TreeIdBare)
      -- ^ The partition to restore (or remove, if `Nothing`)
    }
  | ReprModif -- ^ Modified a partition
    { treeId : TreeIdBare
      -- ^ To which tree ID does it refer
    , restoreRepr : TreeIdBare
      -- ^ The representative to restore
    }
  | SentModif -- ^ Modified a sentence
    { treeId : TreeIdBare
      -- ^ To which tree ID does it refer
    , restoreSent : Sent
      -- ^ The sentel to restore
    }


-- | Save the given atomic modification in the editing history.
saveModif : FileId -> HistAtom -> Model -> Model
saveModif fileId atom model =
    let
        update info =
            { info
                | undoLast = atom :: info.undoLast
                , redoHist = []
            }
    in
        Lens.update (fileInfoLensAlt fileId) update model
--   { model
--       | undoLast = atom :: model.undoLast
--       , redoHist = [] }


-- | Freeze the current (last) sequence of history modifications.
--
-- The idea is that everything that is in `undoLast` will be transfered to
-- `undoHist` as single, atomic element of the editing history. Put differently,
-- the current atomic modifications are put in a transaction.
freezeHist : Model -> Model
freezeHist model =
    let
        histSize = 250 -- should be in Config, but Config relies on the model...
        fileId = getFileId model.focus model
        update info =
            let
                newHist = L.take histSize (info.undoLast :: info.undoHist)
            in
                case info.undoLast of
                    [] -> info
                    _  -> { info
                              | undoHist = newHist
                              , undoLast = []
                          }
    in
        Lens.update (fileInfoLensAlt fileId) update model


-- | Apply a given history atomic element.
applyAtom : FileId -> HistAtom -> Model -> (Model, HistAtom)
applyAtom fileId histAtom model =
  case histAtom of
    TreeModif r ->
      let
        oldTree = getTreeMayAlt fileId r.treeId model
        tmpModel = setTreeAlt_ fileId r.treeId r.restoreTree model
        newModel = focusOnTree model.focus fileId r.treeId tmpModel
        newAtom = TreeModif {r | restoreTree = oldTree}
      in
        (newModel, newAtom)
    LinkModif r ->
      let
        lens = fileLensAlt fileId => linkMap
        newLinks = D.union r.addLinkSet <| D.diff (Lens.get lens model) r.delLinkSet
        chooseTree xs =
            case xs of
                [] -> Nothing
                x :: _ -> Just x
        topTreeId = chooseTree
            <| L.map (\((treeId, _), _) -> treeId)
            <| D.keys r.addLinkSet ++ D.keys r.delLinkSet
        botTreeId = chooseTree
            <| L.map (\(_, (treeId, _)) -> treeId)
            <| D.keys r.addLinkSet ++ D.keys r.delLinkSet
        maybe f mayArg x =
            case mayArg of
                Just arg -> f arg x
                Nothing  -> x
        newModel =
            maybe (focusOnTree Top fileId) topTreeId <|
            maybe (focusOnTree Bot fileId) botTreeId <|
            Lens.set lens newLinks model
        newAtom = LinkModif
                  { addLinkSet = r.delLinkSet
                  , delLinkSet = r.addLinkSet }
      in
        (newModel, newAtom)
    PartModif r ->
      let
        oldPart = getPartAlt fileId r.treeId model
        -- NOTE: we could simply use `setPart`, but we need to return a `newAtom`...
        tmpModel = setPartAlt_ fileId r.treeId r.restorePart model
        newModel = focusOnTree model.focus fileId r.treeId tmpModel
        newAtom = PartModif {r | restorePart = oldPart}
      in
        (newModel, newAtom)
    ReprModif r ->
      let
        oldRepr = getReprAlt fileId r.treeId model
        tmpModel = Lens.update
                   (fileLensAlt fileId => reprMap)
                   (D.insert r.treeId r.restoreRepr) model
        newModel = focusOnTree model.focus fileId r.treeId tmpModel
        newAtom = ReprModif {r | restoreRepr = oldRepr}
      in
        (newModel, newAtom)
    SentModif r ->
      let
        oldSent = getSentAlt_ fileId r.treeId model
        newModel = Lens.update
                   (fileLensAlt fileId => sentMap)
                   (D.insert r.treeId r.restoreSent) model
        newAtom = SentModif {r | restoreSent = oldSent}
      in
        (newModel, newAtom)


-- | Focus on the given tree if needed.
focusOnTree : Focus -> FileId -> PartId -> Model -> Model
focusOnTree focus fileId treeId model =
    let
        topTree = Lens.get (windowLens Top => tree) model
        botTree = Lens.get (windowLens Bot => tree) model
    in
        case focus of
            Top ->
                if getReprId focus topTree model == treeId &&
                   getFileId focus model == fileId
                then model
                else moveToTree focus fileId treeId model
            Bot ->
                if getReprId focus botTree model == treeId &&
                   getFileId focus model == fileId
                then model
                else moveToTree focus fileId treeId model


-- | Apply a given history element.
applyElem : FileId -> HistElem -> Model -> (Model, HistElem)
applyElem fileId elem =
  let
    apply xs model histAcc = case xs of
      [] -> (model, histAcc)
      hd :: tl ->
        let (newModel, newAtom) = applyAtom fileId hd model
        in  apply tl newModel (newAtom :: histAcc)
  in
    -- \model -> Debug.log (toString elem) <| apply elem model []
    \model -> apply elem model []


-- | Perform undo in the active workspace.
undo : Model -> Model
undo model =
  let
      fileId = getFileId model.focus model
      info = Lens.get (fileInfoLensAlt fileId) model
  in
      case info.undoHist of
        [] -> model
        histElem :: histRest ->
          let
              (newModel, newElem) = applyElem fileId histElem model
              update info =
                  { info
                      | undoHist = histRest
                      , redoHist = newElem :: info.redoHist
                  }
          in
              Lens.update (fileInfoLensAlt fileId) update newModel


-- | Perform redo in the active workspace.
redo : Model -> Model
redo model =
  let
      fileId = getFileId model.focus model
      info = Lens.get (fileInfoLensAlt fileId) model
  in
      case info.redoHist of
        [] -> model
        histElem :: histRest ->
          let
              (newModel, newElem) = applyElem fileId histElem model
              update info =
                  { info
                      | redoHist = histRest
                      , undoHist = newElem :: info.undoHist
                  }
          in
              Lens.update (fileInfoLensAlt fileId) update newModel


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
-- Sentence-wise
---------------------------------------------------


-- | Set the sentence under a given ID.  A tricky function which seems trivial...
-- Returns `Nothing` if it fails for some reasons.
setSent : Focus -> PartId -> Sent -> Model -> Maybe Model
setSent focus partId newSent model =
    let
        sentMap = getSentMap focus partId model
    in
        case reAlignSent (D.values sentMap) newSent of
            Nothing -> Nothing
            Just newSentList ->
                let update (treeId, new) = setSent_ focus treeId new
                in  Just <|
                    L.foldl update model <|
                    L.map2 (,) (D.keys sentMap) newSentList


-- | Set a single sentence under a given ID.
setSent_ : Focus -> TreeIdBare -> Sent -> Model -> Model
setSent_ focus treeId sent model =
    let
        oldSent = getSent_ focus treeId model
        newModel = Lens.update (fileLens focus => sentMap) (D.insert treeId sent) model
    in
        newModel
            |> saveModif (getFileId focus model)
               ( SentModif
                 { treeId = treeId
                 , restoreSent = oldSent }
               )


-- | Take (a) a list of sentences and (b) a sentence, such that concatenated (a)
-- and (b) represent a tokenization of the same text, and split (b) in such
-- places that the resulting list of sentences is aligned with (a) (i.e. both
-- lists have the same length and their respective elements correspond to the
-- same chunks of text).
reAlignSent : List Sent -> Sent -> Maybe (List Sent)
reAlignSent chunks sent =
--   Debug.log (toString chunks) <|
--   Debug.log (toString sent) <|
  case chunks of
      [] ->
          if L.isEmpty sent
          then Just []
          else Nothing
      chunk :: chunksRest ->
          case divideWithPrefix (sentToString chunk) sent of
              Nothing -> Nothing
              Just (pref, suff) ->
                  let cons x xs = x :: xs
                  in  Maybe.map (cons pref) (reAlignSent chunksRest suff)


-- | Divide the given sentence with the given prefix so that the first element
-- of the resulting pair corresponds to the prefix and the second -- to the
-- suffix.
divideWithPrefix : String -> Sent -> Maybe (Sent, Sent)
divideWithPrefix txt sent =
    if Str.isEmpty txt
    then Just ([], sent)
    else
        case sent of
            [] -> Nothing
            tok :: sentRest ->
                case stripPrefix tok.orth txt of
                    Nothing -> Nothing
                    Just suff ->
                        let onFirst f (x, y) = (f x, y)
                            cons x xs = x :: xs
                        in  Maybe.map
                            (onFirst <| cons tok)
                            (divideWithPrefix (Str.trimLeft suff) sentRest)


-- | Strip the prefix and return the suffix.
stripPrefix : String -> String -> Maybe String
stripPrefix pref x =
    if Str.startsWith pref x
    then Just <| Str.dropLeft (Str.length pref) x
    else Nothing


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


-- | Set the tree under a given ID. Does not require that the tree already
-- exists (in contrast to `updateTree`, for example).
-- NOTE: this is not really a high-level function, from the high-level point of
-- view it should be not possible to remove a tree. Put differently, this
-- function leaves the model in an invalid state!
setTree : Focus -> PartId -> Maybe (R.Tree Node) -> Model -> Model
setTree win treeId newTree model =

  let

    -- Calculate the new tree and update the model
    fileId = getFileId win model
    oldTree = getTreeMayAlt fileId treeId model
    newModel = setTreeAlt_ fileId treeId newTree model

    -- Delete the corresponding links, if needed. We assume that the new tree
    -- has nothing in common with the previous one, therefore, all previous
    -- links get deleted.
    isToDelete ((from, to), linkData_) =
      let toDel (trId, ndId_) = trId == treeId
      in  toDel from || toDel to
    -- delLinks = L.filter isToDelete <| D.toList model.file.linkMap
    delLinks = L.filter isToDelete <| D.toList (Lens.get (fileLens win) model).linkMap
    linkModif = deleteLinks win (S.fromList <| L.map Tuple.first delLinks)

  in

     newModel
       |> linkModif
       |> saveModif fileId
          ( TreeModif
            { treeId = treeId
            , restoreTree = oldTree }
          )


-- | Update the tree under a given ID.
updateTree
    : Focus -> PartId
    -> (R.Tree Node -> R.Tree Node)
    -> Model -> Model
updateTree win treeId update model =

  let

    -- calculate the new tree and update the model
    oldTree = getTree win treeId model
    newModel = updateTree_ win treeId (Just << update) model
    newTree = getTree win treeId newModel

    -- we also calculate the set of deleted nodes
    delNodes = S.diff (nodesIn oldTree) (nodesIn newTree)

    -- and delete the corresponding links, if needed
    isToDelete ((from, to), linkData_) =
      let toDel (trId, ndId) = trId == treeId && S.member ndId delNodes
      in  toDel from || toDel to
    -- delLinks = L.filter isToDelete <| D.toList model.file.linkMap
    delLinks = L.filter isToDelete <| D.toList (Lens.get (fileLens win) model).linkMap
    linkModif = deleteLinks win (S.fromList <| L.map Tuple.first delLinks)

  in

     newModel
       |> linkModif
       |> saveModif (getFileId win model)
          ( TreeModif
            { treeId = treeId
            , restoreTree = Just oldTree }
          )


-- | An internal version of `updateTree`, which does *not* update the history.
updateTree_
    : Focus -> PartId
    -> (R.Tree Node -> Maybe (R.Tree Node))
    -> Model -> Model
updateTree_ win treeId update model =
  let
    alter v = case v of
      Nothing -> Debug.crash "Model.updateTree_: no tree with the given ID"
      Just tree -> update tree
    newTrees = D.update treeId alter
    newModel = Lens.update (fileLens win => treeMap) newTrees model
  in
    newModel


-- | An internal version of `setTree`, which does *not* update the history.
setTree_
    : Focus -> PartId
    -> Maybe (R.Tree Node)
    -> Model -> Model
setTree_ win treeId treeMay model =
    setTreeAlt_ (getFileId win model) treeId treeMay model


-- | A version of `setTree_`.
setTreeAlt_
    : FileId -> PartId
    -> Maybe (R.Tree Node)
    -> Model -> Model
setTreeAlt_ fileId treeId treeMay model =
  let
    newTrees treeMap =
        case treeMay of
            Nothing -> D.remove treeId treeMap
            Just tr -> D.insert treeId tr treeMap
    newModel = Lens.update (fileLensAlt fileId => treeMap) newTrees model
  in
    newModel


-- | Return the set of node IDs in the given tree.
nodesIn : R.Tree Node -> S.Set NodeId
nodesIn = S.fromList << L.map (Lens.get nodeId) << R.flatten


---------------------------------------------------
-- Removal
---------------------------------------------------


-- | Remove the tree under a given ID.
removeTree : Focus -> PartId -> Model -> Model
removeTree win treeId = setTree win treeId Nothing


---------------------------------------------------
-- Link-wise
---------------------------------------------------


-- | Delete the set of links.
deleteLinks : Focus -> S.Set Link -> Model -> Model
deleteLinks win delLinks model =
  let
    linksLens = fileLens win => linkMap
    linkData link = case D.get link (Lens.get linksLens model) of
      Nothing -> Nothing
      Just data -> Just (link, data)
    oldLinks = D.fromList <| Util.catMaybes <| L.map linkData (S.toList delLinks)
  in
    if S.isEmpty delLinks
    then model
    else Lens.update linksLens (\s -> D.diff s oldLinks) model
      |> saveModif (getFileId win model)
         ( LinkModif
           { addLinkSet = oldLinks
           , delLinkSet = D.empty }
         )


-- -- | Add links.
-- connect : Model -> Model
-- connect model = model |>
--   case (model.focus, model.top.selMain, model.bot.selMain) of
--     (Top, Just topNode, Just botNode) ->
--       connectHelp {nodeFrom = botNode, nodeTo = topNode, focusTo = Top}
--     (Bot, Just topNode, Just botNode) ->
--       connectHelp {nodeFrom = topNode, nodeTo = botNode, focusTo = Bot}
--     _ -> identity
--     -- _ -> Debug.crash "ALALALAL"


-- | Add links.
connect : Model -> Model
connect model =
  case model.selLink of
    Nothing -> connectNodes model
    Just link -> connectSignal link model.focus model


-- | Add links.
connectNodes : Model -> Model
connectNodes model = model |>
  case ( Lens.get (windowLens Top => selMain) model
       , Lens.get (windowLens Bot => selMain) model ) of
  -- case (model.top.selMain, model.bot.selMain) of
    (Just topNode, Just botNode) ->
      connectHelp {nodeFrom = topNode, nodeTo = botNode, focusTo = Bot}
    _ -> identity


type alias LinkInfo =
  { nodeFrom : NodeId
  , nodeTo : NodeId
  , focusTo : Focus }


-- | The operation works only in single annotation mode.
connectHelp : LinkInfo -> Model -> Model
connectHelp {nodeFrom, nodeTo, focusTo} model =
  let
    focusFrom = case focusTo of
      Top -> Bot
      Bot -> Top
    fileFrom = Lens.get (top => fileId) model
    fileTo   = Lens.get (bot => fileId) model
    treeFrom = getReprId Top (selectWin focusFrom model).tree model
    treeTo   = getReprId Top (selectWin focusTo model).tree model
    link = ((treeFrom, nodeFrom), (treeTo, nodeTo))
    -- TODO: THE CODE BELOW HAS TO CHANGE!
    (alter, modif) =
      ( identity
      , LinkModif
            { delLinkSet = D.empty
            , addLinkSet = D.empty }
      )
--             Nothing ->
--               ( D.insert link linkDataDefault
--               , LinkModif
--                     { addLinkSet = D.empty
--                     , delLinkSet = D.singleton link linkDataDefault }
--               )
--             Just linkData ->
--               ( D.remove link
--               , LinkModif
--                     { delLinkSet = D.empty
--                     , addLinkSet = D.singleton link linkData }
--               )
  in
      if fileFrom == fileTo
      then Lens.update (fileLens Top => linkMap) alter model
          |> saveModif fileTo modif
      else model


-- | Connect a link to a signal.
connectSignal
  : Link   -- ^ The link to connect with a signal
  -> Focus -- ^ Focus on the window with the signal
  -> Model
  -> Model
connectSignal link focus model =
  let
    win = selectWin focus model
    treeId = getReprId focus win.tree model
  in
    case win.selMain of
      Nothing -> model
      Just nodeId -> addSignal link focus (treeId, nodeId) model


-- | Connect a link to a signal.
addSignal
  : Link   -- ^ The link to connect with the signal
  -> Focus
  -> Addr  -- ^ The address of the signal
  -> Model
  -> Model
addSignal link focus signal model =
  let
    fileFrom = Lens.get (top => fileId) model
    fileTo   = Lens.get (bot => fileId) model
    (alter, modif) = case D.get link (Lens.get (fileLens Top => linkMap) model) of
      _ -> Debug.crash "addSignal: implement!"
--       Nothing ->
--         Debug.crash "addSignal: should never happen!"
--       Just oldData ->
--         let
--           newData = switchSignal signal oldData
--         in
--           ( D.insert link newData
--           , LinkModif
--               { delLinkSet = D.singleton link newData
--               , addLinkSet = D.singleton link oldData }
--           )
  in
    if fileFrom == fileTo
    then Lens.update (fileLens Top => linkMap) alter model |> saveModif fileTo modif
    else model

---------------------------------------------------
-- Selection-wise?
---------------------------------------------------


-- | Update the selections.
updateSelection : Model -> Model
updateSelection =
  updateSelection_ Top >>
  updateSelection_ Bot >>
  updateLinkSelection


-- | Update the selection in the given window.
updateSelection_ : Focus -> Model -> Model
updateSelection_ focus model =
  let
    -- wlen = winLens focus
    wlen = windowLens focus
    win = Lens.get wlen model
    tree = getTree focus (getReprId focus win.tree model) model
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


-- | Update the relation-related selection.
updateLinkSelection : Model -> Model
updateLinkSelection model =

  let

    inWin focus (partId, nodeId) =
      let
        win = selectWin focus model
        winPartId = getReprId focus win.tree model
        tree = getTree focus winPartId model
      in
        winPartId == partId && Util.isJust (getNode_ nodeId tree)

    inTop = inWin Top
    inBot = inWin Bot
    inView (from, to) =
      (inTop from && inBot to) ||
      (inBot from && inTop to)

    newLink = case model.selLink of
      Nothing -> Nothing
      Just x -> Util.check inView x

  in

    {model | selLink = newLink}


-- | Set the selection of the given window to the given node IDs. There's no
-- relation between `setSelection` and `updateSelection`, really.
setSelection : S.Set NodeId -> Focus -> Model -> Model
setSelection ids focus =
    let
        updateWin win =
            { win
            | selAux = ids
            , selMain = Nothing }
    in
        Lens.update (windowLens focus) updateWin


---------------------------------------------------
---------------------------------------------------
-- Misc
---------------------------------------------------
---------------------------------------------------


-- | Return the window in focus.
selectWin : Focus -> Model -> Window
selectWin focus = Lens.get (windowLens focus)
--   case focus of
--     Top -> model.top
--     Bot -> model.bot


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


-- -- | Get a partition representative for a given treeID.
-- getReprId : TreeId -> Model -> PartId
-- getReprId (TreeId treeId) model =
--   case D.get treeId model.file.partMap of
--     Nothing -> Debug.crash "Model.getReprId: no partition for the given ID"
--     Just st ->
--         case S.toList st of
--             xMin :: _ -> xMin
--             [] -> Debug.crash "Model.getReprId: empty partition"


-- | Get a partition representative for a given treeID.
getReprId : Focus -> TreeId -> Model -> PartId
getReprId win (TreeId treeId) model =
    let
      rmap = Lens.get (fileLens win => reprMap) model
    in
      case D.get treeId rmap of
        Nothing -> Debug.crash "Model.getReprId: no representative for the given ID"
        Just bareId ->
            if bareId == treeId
            then bareId
            else getReprId win (TreeId bareId) model


-- | Get the ID of the k-th partition.
-- For a reversed version, see `treePos`.
getPartId : Focus -> Int -> Model -> PartId
getPartId win ix0 model =
    let
        pmap = Lens.get (fileLens win => partMap) model
        go ix ps =
            case ps of
                [] -> Debug.crash "Model.getPartId: empty partition map"
                partId :: [] -> partId
                partId :: partRest ->
                    if ix <= 0
                    then partId
                    else go (ix-1) partRest
    in
        go ix0 (D.keys pmap)


-- | Get a tree under a given ID.
getTree : Focus -> PartId -> Model -> R.Tree Node
getTree win treeId model =
    case getTreeMay win treeId model of
      Nothing -> Debug.crash <| "Model.getTree: no tree with the ID " ++ toString treeId
      Just t -> t


-- | Get a tree under a given ID.  A version of `getTree` which does not
-- assume that the tree exists (which is wrong to assume if, e.g., we undo
-- a previous removal).
getTreeMay : Focus -> PartId -> Model -> Maybe (R.Tree Node)
getTreeMay win treeId model =
    getTreeMayAlt (getFileId win model) treeId model


-- | A version of `getTreeMay`.
getTreeMayAlt : FileId -> PartId -> Model -> Maybe (R.Tree Node)
getTreeMayAlt fileId treeId model =
    D.get treeId <| Lens.get (fileLensAlt fileId => treeMap) model


-- | Retrieve all selected nodes.
selAll : Window -> S.Set NodeId
selAll win =
  S.union win.selAux <|
    case win.selMain of
      Nothing -> S.empty
      Just x  -> S.singleton x


-- | Change the tree in focus, provided that it has the appropriate file and
-- tree IDs.
setTreeCheck : FileId -> PartId -> R.Tree Node -> Model -> Model
setTreeCheck fileId treeId tree model =
  let
    focus = model.focus
    win = selectWin focus model
    treeIdSel = getReprId focus win.tree model
    fileIdSel = getFileId focus model
  in
    if fileId == fileIdSel && treeId == treeIdSel
    then updateTree focus treeIdSel (\_ -> tree) model
    else model


-- | Change the tree in focus, provided that it has the appropriate file and
-- tree IDs.  In contrast to `setTreeCheck`, the function takes the list of
-- (optional) trees.
--
-- NOTE: the function recomputes the IDs of the nodes.
--
-- TODO: We should probably check that the restul is sound.
setForestCheck : FileId -> PartId -> List (Maybe (R.Tree Node)) -> Model -> Model
setForestCheck fileId treeId newForest0 model =
  let
    focus = model.focus
    win = selectWin focus model
    treeIdSel = getReprId focus win.tree model
    fileIdSel = getFileId focus model
    oldForest = R.subTrees <| getTree focus treeIdSel model
    newForest =
        flip L.map (L.map2 (,) oldForest newForest0) <|
            \(oldTree, mayNewTree) ->
                case mayNewTree of
                    Just newTree -> newTree
                    _ -> oldTree
    sentNode = Node {nodeId=0, nodeVal="ROOT", nodeTyp=Nothing, nodeComment=""}
    newTree = reID <| R.Node sentNode newForest
  in
    if fileId == fileIdSel && treeId == treeIdSel
    then updateTree focus treeIdSel (\_ -> newTree) model
    else model


-- | Retrieve the sentence corresponding to the given partition.
getSent : Focus -> PartId -> Model -> Sent
getSent focus partId = L.concat << D.values << getSentMap focus partId


-- | Get the sub-sentence corresponding to the given ID and the token ID
-- corresponding to its first token.
getSubSent : Focus -> TreeId -> Model -> (Sent, Int)
getSubSent focus treeId model =
    let
        partId = getReprId focus treeId model
        go tokID sentList =
            case sentList of
                [] -> Debug.crash "Model.getSubSent: impossible happened"
                (bareId, sent) :: rest ->
                    if bareId == unTreeId treeId
                    then (sent, tokID)
                    else go (tokID + L.length sent) rest
    in
        go 0 <| D.toList <| getSentMap focus partId model


-- | Retrieve the sentences corresponding to the given partition.
getSentMap : Focus -> PartId -> Model -> D.Dict TreeIdBare Sent
getSentMap win partId model =
    let
        part = Maybe.withDefault S.empty <| getPart win partId model
        sents = L.map
                (\treeId -> (treeId, getSent_ win treeId model))
                (S.toList part)
    in
        D.fromList sents


-- | Get the sentence corresponding to the given ID.
getSent_ : Focus -> TreeIdBare -> Model -> Sent
getSent_ win treeId model =
    getSentAlt_ (getFileId win model) treeId model


-- | Get the sentence corresponding to the given ID.
getSentAlt_ : FileId -> TreeIdBare -> Model -> Sent
getSentAlt_ fileId treeId model =
    case D.get treeId (Lens.get (fileLensAlt fileId) model).sentMap of
        Nothing -> Debug.crash "Model.getSentAlt_: got Nothing"
        Just sent -> sent

-- | Get the token on the given position in the given partition.
getToken : Int -> Focus -> PartId -> Model -> Token
getToken pos win partId model =
    case getTokenMay pos win partId model of
        Nothing -> Debug.crash "Model.getToken: got Nothing"
        Just tok -> tok


-- | Get the token on the given position in the given partition.
getTokenMay : Int -> Focus -> PartId -> Model -> Maybe Token
getTokenMay pos win partId model =
    Util.at pos (getSent win partId model)


-- | Change the sentence in focus, provided that it has the appropriate
-- file and tree IDs.
setSentCheck : FileId -> PartId -> Sent -> Model -> Model
setSentCheck fileId treeId sent model =
  let
    focus = model.focus
    win = selectWin focus model
    treeIdSel = getReprId focus win.tree model
    fileIdSel = getFileId focus model
  in
    if fileId == fileIdSel && treeId == treeIdSel
    then Maybe.withDefault model <| setSent focus treeIdSel sent model
    else model


---------------------------------------------------
-- Position
---------------------------------------------------


treePos : Focus -> Model -> Int
treePos win model =
  let
    treeId = Lens.get (windowLens win => tree) model
--     treeId = case win of
--       Top -> model.top.tree
--       Bot -> model.bot.tree
    partId = getReprId win treeId model
    go i keys = case keys of
      [] -> 0
      hd :: tl -> if partId == hd
        then i
        else go (i + 1) tl
  in
    go 1 <| D.keys <| Lens.get (fileLens win => treeMap) model


-- | Number of trees in the model. Note that it does not necessarily correspond
-- to the number of turns or turn elements.
treeNum : Focus -> Model -> Int
-- treeNum model = D.size model.file.treeMap
treeNum win model = D.size (Lens.get (fileLens win) model).treeMap


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


-- getPosition : Window -> Position
-- getPosition win =
--   case win.drag of
--     Just {start, current} ->
--       Position
--         (win.pos.x + current.x - start.x)
--         (win.pos.y + current.y - start.y)
--     Nothing -> win.pos


-- | The position of the root node of the tree in the workspaces determined by
-- the given focus value.
getPosition : Focus -> Model -> Position
getPosition foc model =
    let
        win = Lens.get (windowLens foc) model
        ws  = Lens.get (workspaceLens foc) model
    in
        case ws.drag of
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
nextTree : Focus -> TreeId -> Model -> PartId
nextTree focus x model =
    nextTree_ focus (getReprId focus x model) model


-- | Retrieve the next tree in the underlying model.
-- Return the argument tree ID if not possible.
nextTree_ : Focus -> PartId -> Model -> PartId
nextTree_ focus x model =
  let
    go keys = case keys of
      [] -> x
      hd1 :: []  -> x
      hd1 :: hd2 :: tl -> if x == hd1
        then hd2
        else go (hd2 :: tl)
  in
    go <| D.keys <| Lens.get (fileLens focus => treeMap) model


-- | Retrieve the next tree in the underlying model.
-- Return the argument tree ID if not possible.
prevTree : Focus -> TreeId -> Model -> PartId
prevTree focus x0 model =
  let
    x = getReprId focus x0 model
    go keys = case keys of
      [] -> x
      hd1 :: []  -> x
      hd1 :: hd2 :: tl -> if x == hd2
        then hd1
        else go (hd2 :: tl)
  in
    go <| D.keys <| Lens.get (fileLens focus => treeMap) model


-- | Wrapper for prevTree and nextTree.
moveCursor : Bool -> Model -> Model
moveCursor next model =
  let
    win = selectWin model.focus model
    switch = if next then nextTree else prevTree
    treeId = switch model.focus win.tree model
  in
    moveCursorTo model.focus treeId model


-- | Similar to `moveCursor`, but the tree ID as well as the focus are directly
-- specified.
moveCursorTo : Focus -> PartId -> Model -> Model
moveCursorTo focus treeId model =
  let
    alter win =
      { win
          | tree = TreeId treeId
          , selMain = Nothing
          , selAux = S.empty
      }
  in
    Lens.update (windowLens focus) alter model


-- | Move the given workspace to the given file.
moveToFile : Focus -> FileId -> Model -> Model
moveToFile focus fileId model =
  let
    alterWS ws =
      { ws
          | fileId = fileId
          , drag = Nothing
      }
  in
    Lens.update (workspaceLens focus) alterWS model


-- | Move the given workspace to the given file and tree.
moveToTree : Focus -> FileId -> PartId -> Model -> Model
moveToTree focus fileId treeId model =
  let
    alterWin win =
      { win
          | tree = TreeId treeId
          , selMain = Nothing
          , selAux = S.empty
      }
    alterWS ws =
      { ws
          | fileId = fileId
          , drag = Nothing
      }
  in
    Lens.update (windowLens focus) alterWin <|
    Lens.update (workspaceLens focus) alterWS model


-- | Move the given workspace to the given file and its first tree.
moveToFirst : Focus -> FileId -> Model -> Model
moveToFirst focus fileId model =
    let
        trees = Lens.get (fileLensAlt fileId => treeMap) model
    in
        case D.toList trees of
            [] -> Debug.crash "moveToFirst: empty treeMap"
            (partId, tree_) :: _ -> moveToTree focus fileId partId model


-- | A version of `moveToTree` which tries to move to the tree (partition) under
-- the given index.
moveToTreeIx : Focus -> FileId -> Int -> Model -> Model
moveToTreeIx focus fileId partIx model0 =
  let
    model = moveToFile focus fileId model0
    partId = getPartId focus partIx model
    alterWin win =
      { win
          | tree = TreeId partId
          , selMain = Nothing
          , selAux = S.empty
      }
  in
    Lens.update (windowLens focus) alterWin model


---------------------------------------------------
-- Swapping
---------------------------------------------------


-- | Swap to the next file in the given workspace.
swapFile : Focus -> Model -> Model
swapFile focus model =
    let
        fileId = getFileId focus model
        findNext xs = case xs of
            [] -> Debug.crash "swapFile: empty fileList"
            (fid, _) :: rest ->
                if fileId == fid
                then takeFirst rest
                else findNext rest
        takeFirst xs = case xs of
            [] -> Debug.crash "swapFile.takeFirst: empty list"
            (newFid, _) :: _ -> newFid
    in
        let files = model.fileList ++ model.fileList
        in  moveToFile focus (findNext files) model


-- | Swap the top and bottom workspaces
swapWorkspaces : Model -> Model
swapWorkspaces =
    let
        swap x = {x | top = x.bot, bot = x.top}
        swapInfos model =
            let newFileList = L.map (onSecond swap) model.fileList
            in  {model | fileList = newFileList}
        onSecond f (x, y) = (x, f y)
    in
        swapInfos << swap


-- | In each workspace, move to the next file and try to focus on the current
-- tree (i.e., the one in the current file). The command should be especially
-- useful in the adjudication mode, when relations are to be compared.
swapFiles : Model -> Model
swapFiles =
    swapFileSaveTree Top <<
    swapFileSaveTree Bot


-- | A version of `swapFile` which tries to stay focus on the same tree, even
-- though the file is changed. Useful for `swapFiles`.
swapFileSaveTree : Focus -> Model -> Model
swapFileSaveTree focus model =
    let

        fileId = getFileId focus model
        findNext xs = case xs of
            [] -> Debug.crash "swapFile: empty fileList"
            (fid, _) :: rest ->
                if fileId == fid
                then takeFirst rest
                else findNext rest
        takeFirst xs = case xs of
            [] -> Debug.crash "swapFile.takeFirst: empty list"
            (newFid, _) :: _ -> newFid

        partId = getReprId focus (selectWin focus model).tree model
        partIx = treePos focus model - 1

    in

        let files = model.fileList ++ model.fileList
        in  moveToTreeIx focus (findNext files) partIx model


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
    update (windowLens focus)
--     case focus of
--       Top -> update top
--       Bot -> update bot


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
    update (windowLens focus)
--     case focus of
--       Top -> update top
--       Bot -> update bot


---------------------------------------------------
-- Nodes
---------------------------------------------------


-- | TODO: this function is too high level, it should be probably discarded or
-- moved elsewhere (while the actual `getNode_` should take its place).
getNode : NodeId -> Focus -> Model -> Node
getNode id focus model =
  let
    partId = getReprId focus (selectWin focus model).tree model
    tree = getTree focus partId model
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


---------------------------------------------------
-- Node Type
---------------------------------------------------


-- | A type of a node
type NodeTyp
    = Terminal
    | Phrasal
    | PosNode
      -- ^ Part-of-speech level node
    | Internal
      -- ^ An internal node for which the program is not able to tell whether
      -- it's POS or phrasal


-- | Determine the type of the node, return `Nothing` if the argument node ID is
-- invalid.
getNodeTyp : NodeId -> R.Tree Node -> Maybe NodeTyp
getNodeTyp id tree =
  let
    search (R.Node x ts) = if id == Lens.get nodeId x
      then Just (getRootTyp <| R.Node x ts)
      else searchF ts
    searchF ts = case ts of
      [] -> Nothing
      hd :: tl -> Util.mappend (search hd) (searchF tl)
  in
    search tree


-- | Get the type of the root of the given subtrees.
getRootTyp : R.Tree Node -> NodeTyp
getRootTyp tree =
    case tree of
        R.Node (Leaf _) _ -> Terminal
        R.Node _ ts ->
            let
                minD = forestDepth L.minimum ts
                maxD = forestDepth L.maximum ts
            in
                if minD == 1 && maxD == 1
                then PosNode
                else if minD == 1
                     then Internal
                     else Phrasal


-- | (Minimum / maximum, depending on the argument function) depth of the given tree.
treeDepth : (List Int -> Maybe Int) -> R.Tree a -> Int
treeDepth minMax = (\x -> x+1) << forestDepth minMax << R.subTrees


-- | (Minimum / maximum) depth of the given forest.
forestDepth : (List Int -> Maybe Int) -> R.Forest a -> Int
forestDepth minMax = Maybe.withDefault 0 << minMax << L.map (treeDepth minMax)


---------------------------------------------------
-- Node modification
---------------------------------------------------


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
    treeId = getReprId focus win.tree model
  in
    updateTree focus treeId update model


setNode : NodeId -> Focus -> Node -> Model -> Model
setNode id focus newNode = updateNode id focus (\_ -> newNode)


---------------------------------------------------
-- Labels
---------------------------------------------------


-- | Get label of a given node.  Works for both non-terminals and terminals.
getLabel : NodeId -> Focus -> Model -> String
getLabel id focus model =
    Lens.get nodeVal <| getNode id focus model
--     let
--         node = getNode id focus model
--     in
--         case node of
--             Node r -> r.nodeVal
--             Leaf r ->
--                 let partId = getReprId (selectWin focus model).tree model
--                 in  (getToken r.leafPos partId model).orth


-- | NOTE: will siltently fail for terminal nodes.
setLabel : NodeId -> Focus -> String -> Model -> Model
setLabel id focus newLabel model =
    let update = Lens.set nodeVal newLabel
    in  updateNode id focus update model


---------------------------------------------------
-- Comments
---------------------------------------------------


getComment : NodeId -> Focus -> Model -> String
getComment id focus model =
    Lens.get nodeComment <| getNode id focus model


setComment : NodeId -> Focus -> String -> Model -> Model
setComment id focus newComment model =
    let update = Lens.set nodeComment newComment
    in  updateNode id focus update model


---------------------------------------------------
-- Attribute and type modification
---------------------------------------------------


-- | Not only sets the type of the entity in focus, but also clears the
-- attributes inconsistent with the new type.
setEntityType : NodeId -> Focus -> String -> Model -> Model
setEntityType id focus newTyp model =
    let
        lensEnt = nodeTyp => maybeLens
        updateEnt ent =
            let
                -- Entity configuration
                cfg = AnnoCfg.entityConfig ent.name model.annoConfig
                -- Old and new type-dependent attributes
                getAtts typ =
                    D.fromList <|
                    Maybe.withDefault [] <|
                    D.get typ cfg.attributesOnType
                oldAtts = getAtts ent.typ
                newAtts = getAtts newTyp
                -- High-level update functions
                updateType = Lens.set Anno.entityType newTyp
                remAtts ent0 = List.foldl
                    (\attr -> Lens.set (Anno.entityAttr attr) Nothing)
                    ent0 (D.keys <| D.diff oldAtts newAtts)
                addAtts ent0 = List.foldl
                    (\(attr, attrCfg) ->
                         Lens.set (Anno.entityAttr attr) (Anno.defaultAttr attrCfg))
                    ent0 (D.toList <| D.diff newAtts oldAtts)
            in
                updateType << addAtts << remAtts <| ent
        update = Lens.update lensEnt updateEnt
    in
        updateNode id focus update model


setEntityAttr : (Lens.Focus Anno.Entity a) -> NodeId -> Focus -> a -> Model -> Model
setEntityAttr attLens id focus newVal model =
    let lens = nodeTyp => maybeLens => attLens
        update = Lens.set lens newVal
    in  updateNode id focus update model


---------------------------------------------------
-- Attribute and type modification
---------------------------------------------------


-- | Set the anchor (timex) of the given node to the selected node.
-- Generic version.
--
-- The process of deciding which node should be considered as the anchor
-- is as follows:
--
-- 1. If there is another node selected in focus, choose it
-- 2. Otherwise, choose the main selected node in the other window
-- 3. Otherwise, do nothing (but return the error message)
setEntityAnchor
    : (Lens.Focus Anno.Entity (Maybe Anno.Attr))
    -> NodeId
    -> Focus
    -> Model
    -> Either String Model
setEntityAnchor attLens id focus model =
    let
        lens = nodeTyp => maybeLens => attLens
        update newVal = Lens.set lens (Maybe.map Anno.Anchor newVal)
        anchorMaybe = or anchorInFocus anchorNoFocus
        or x y = case x of
            Nothing -> y
            _ -> x
        win = selectWin focus model
        anchorInFocus =
            case S.toList win.selAux of
                [x] -> Just (getReprId focus win.tree model, x)
                _   -> Nothing
        anchorNoFocus =
            let
                otherFocus = case focus of
                    Top -> Bot
                    Bot -> Top
                otherWin = selectWin otherFocus model
            in
                Util.guard (model.top.fileId == model.bot.fileId)
                  |> Maybe.andThen (\_ ->
                    case otherWin.selMain of
                      Just x  -> Just
                        (getReprId otherFocus otherWin.tree model, x)
                      Nothing -> Nothing
                                   )
        isTyped addr =
            case R.label (subTreeAt addr focus model) of
                Leaf _ -> False
                Node r -> case r.nodeTyp of
                  Just _  -> True
                  Nothing -> False
    in
        case anchorMaybe of
            Nothing -> Left "To perform anchoring, you have to first either: (i) select an additional node in focus, or (ii) select a node in the other window. Note also that nodes from two different files cannot be linked."
            Just anchor ->
                if isTyped anchor
                then Right <| updateNode id focus (update anchorMaybe) model
                else Left "The selected node is untyped (not a TIMEX, EVENT, ...)"


-- -- | Set the anchor (timex) of the given node to the selected node.
-- setAnchor
--     :  String -- ^ The name of the anchoring attribute
--     -> NodeId
--     -> Focus
--     -> Model
--     -> Either String Model
-- setAnchor =
--     let lens = nodeTyp => maybeLens => nodeTimex => timexAnchor
--     in  setTimexAnchorGen lens


-- ---------------------------------------------------
-- -- Event modification
-- ---------------------------------------------------
--
--
-- setEventAttr : (Lens.Focus Anno.Event a) -> NodeId -> Focus -> a -> Model -> Model
-- setEventAttr attLens id focus newVal model =
--     let lens = nodeTyp => maybeLens => nodeEvent => attLens
--         update = Lens.set lens newVal
--     in  updateNode id focus update model
--
--
-- ---------------------------------------------------
-- -- Signal modification
-- ---------------------------------------------------
--
--
-- setSignalAttr : (Lens.Focus Anno.Signal a) -> NodeId -> Focus -> a -> Model -> Model
-- setSignalAttr attLens id focus newVal model =
--     let lens = nodeTyp => maybeLens => nodeSignal => attLens
--         update = Lens.set lens newVal
--     in  updateNode id focus update model
--
--
-- ---------------------------------------------------
-- -- Timex modification
-- ---------------------------------------------------
--
--
-- setTimexAttr : (Lens.Focus Anno.Timex a) -> NodeId -> Focus -> a -> Model -> Model
-- setTimexAttr attLens id focus newVal model =
--     let lens = nodeTyp => maybeLens => nodeTimex => attLens
--         update = Lens.set lens newVal
--     in  updateNode id focus update model
--
--
-- setTimexType : NodeId -> Focus -> Anno.TimexType -> Model -> Model
-- setTimexType id focus newVal model =
--     let lensTop = nodeTyp => maybeLens => nodeTimex
--         lensType = lensTop => timexType
--         lensBegin = lensTop => timexBeginPoint
--         lensEnd = lensTop => timexEndPoint
--         lensQuant = lensTop => timexQuant
--         lensFreq = lensTop => timexFreq
--         rmDurationRelated = Lens.set lensBegin Nothing >> Lens.set lensEnd Nothing
--         rmSetRelated = Lens.set lensQuant Nothing >> Lens.set lensFreq Nothing
--         update = Lens.set lensType newVal >>
--             case newVal of
--                 Anno.Duration -> rmSetRelated
--                 Anno.Set -> rmDurationRelated
--                 _ -> rmSetRelated >> rmDurationRelated
--     in  updateNode id focus update model
--
--
-- -- | Set the anchor (timex) of the given node to the selected node.
-- -- Generic version.
-- --
-- -- The process of deciding which node should be considered as the anchor
-- -- is as follows:
-- --
-- -- 1. If there is another node selected in focus, choose it
-- -- 2. Otherwise, choose the main selected node in the other window
-- -- 3. Otherwise, do nothing (should return nothing in this case
-- --    so that we can show popup, perhaps?)
-- -- setTimexAnchorGen :  NodeId -> Focus -> Model -> Either String Model
-- setTimexAnchorGen lens id focus model =
--     Debug.crash "Model.setTimexAnchorGet: implement!"
-- --     let
-- --         -- lens = nodeTyp => maybeLens => nodeTimex => timexAnchor
-- --         update newVal = Lens.set lens newVal
-- --         anchorMaybe = or anchorInFocus anchorNoFocus
-- --         or x y = case x of
-- --             Nothing -> y
-- --             _ -> x
-- --         win = selectWin focus model
-- --         anchorInFocus =
-- --             case S.toList win.selAux of
-- --                 [x] -> Just (getReprId win.tree model, x)
-- --                 _   -> Nothing
-- --         anchorNoFocus =
-- --             let
-- --                 otherFocus = case focus of
-- --                     Top -> Bot
-- --                     Bot -> Top
-- --                 otherWin = selectWin otherFocus model
-- --             in
-- --                 case otherWin.selMain of
-- --                     Just x  -> Just (getReprId otherWin.tree model, x)
-- --                     Nothing -> Nothing
-- --         isTyped addr =
-- --             case R.label (subTreeAt addr model) of
-- --                 Leaf _ -> False
-- --                 Node r -> case r.nodeTyp of
-- --                   Just _  -> True
-- --                   Nothing -> False
-- --                   -- Just (NodeTimex _) -> True
-- --                   -- _ -> False
-- --     in
-- --         case anchorMaybe of
-- --             Nothing -> Left "To perform anchoring, you have to first either: (i) select an additional node in focus, or (ii) select a node in the other window."
-- --             Just anchor ->
-- --                 if isTyped anchor
-- --                 then Right <| updateNode id focus (update anchorMaybe) model
-- --                 else Left "The selected node is untyped (not a TIMEX, EVENT, ...)"
--
--
-- -- | Set the anchor (timex) of the given node to the selected node.
-- setTimexAnchor :  NodeId -> Focus -> Model -> Either String Model
-- setTimexAnchor =
--     let lens = nodeTyp => maybeLens => nodeTimex => timexAnchor
--     in  setTimexAnchorGen lens
-- --     let
-- --         lens = nodeTyp => maybeLens => nodeTimex => timexAnchor
-- --         update newVal = Lens.set lens newVal
-- --         anchorMaybe = or anchorInFocus anchorNoFocus
-- --         or x y = case x of
-- --             Nothing -> y
-- --             _ -> x
-- --         win = selectWin focus model
-- --         anchorInFocus =
-- --             case S.toList win.selAux of
-- --                 [x] -> Just (win.tree, x)
-- --                 _   -> Nothing
-- --         anchorNoFocus =
-- --             let
-- --                 otherFocus = case focus of
-- --                     Top -> Bot
-- --                     Bot -> Top
-- --                 otherWin = selectWin otherFocus model
-- --             in
-- --                 case otherWin.selMain of
-- --                     Just x  -> Just (otherWin.tree, x)
-- --                     Nothing -> Nothing
-- --         isTyped addr =
-- --             case R.label (subTreeAt addr model) of
-- --                 Leaf _ -> False
-- --                 Node r -> case r.nodeTyp of
-- --                   Just _  -> True
-- --                   Nothing -> False
-- --                   -- Just (NodeTimex _) -> True
-- --                   -- _ -> False
-- --     in
-- --         case anchorMaybe of
-- --             Nothing -> Left "To perform anchoring, you have to first either: (i) select an additional node in focus, or (ii) select a node in the other window."
-- --             Just anchor ->
-- --                 if isTyped anchor
-- --                 then Right <| updateNode id focus (update anchorMaybe) model
-- --                 else Left "The selected node is untyped (not a TIMEX, EVENT, ...)"
--
--
-- -- | Set the anchor (timex) of the given node to the selected node.
-- setTimexBeginPoint :  NodeId -> Focus -> Model -> Either String Model
-- setTimexBeginPoint =
--     let lens = nodeTyp => maybeLens => nodeTimex => timexBeginPoint
--     in  setTimexAnchorGen lens
--
--
-- -- | Set the anchor (timex) of the given node to the selected node.
-- setTimexEndPoint :  NodeId -> Focus -> Model -> Either String Model
-- setTimexEndPoint =
--     let lens = nodeTyp => maybeLens => nodeTimex => timexEndPoint
--     in  setTimexAnchorGen lens
--
--
-- -- | Remove the anchor.
-- remTimexAnchor :  NodeId -> Focus -> Model -> Model
-- remTimexAnchor id focus model =
--     let
--         lens = nodeTyp => maybeLens => nodeTimex => timexAnchor
--         update = Lens.set lens Nothing
--     in
--         updateNode id focus update model
--
--
-- -- | Remove the anchor.
-- remTimexBeginPoint :  NodeId -> Focus -> Model -> Model
-- remTimexBeginPoint id focus model =
--     let
--         lens = nodeTyp => maybeLens => nodeTimex => timexBeginPoint
--         update = Lens.set lens Nothing
--     in
--         updateNode id focus update model
--
--
-- -- | Remove the anchor.
-- remTimexEndPoint :  NodeId -> Focus -> Model -> Model
-- remTimexEndPoint id focus model =
--     let
--         lens = nodeTyp => maybeLens => nodeTimex => timexEndPoint
--         update = Lens.set lens Nothing
--     in
--         updateNode id focus update model


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
    treeId = getReprId focus win.tree model
    tree = getTree focus treeId model
    newTree = f (selAll win) tree
  in
    if tree /= newTree
    then updateTree focus treeId (\_ -> newTree) model
    else model


-- | Process selected nodes in a given window.
procSelWithSent
  :  (S.Set NodeId -> Sent -> R.Tree Node -> (Sent, R.Tree Node))
  -> Focus -> Model -> Model
procSelWithSent f focus model =
  let
      win = selectWin focus model
      treeId = getReprId focus win.tree model
      sent = getSent focus treeId model
      tree = getTree focus treeId model
      (newSent, newTree) = f (selAll win) sent tree
  in
      model |>
      updateTree focus treeId (\_ -> newTree) |>
      setSent focus treeId newSent |>
      Maybe.withDefault model


---------------------------------------------------
-- Delete
---------------------------------------------------


-- | Delete selected link or nodes in a given window.
-- Priority is given to links.
deleteSel : Focus -> Model -> Model
deleteSel focus model =
  let
    f ids t = L.foldl deleteNode t (S.toList ids)
  in
    case model.selLink of
      Nothing -> procSel f focus model
      Just x -> deleteLinks focus
        (S.singleton x)
        {model | selLink = Nothing}


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
  let
      f ids t = L.foldl deleteTree t (S.toList ids)
  in
      procSel f


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


---------------------------------------------------
-- Re-identification
---------------------------------------------------


-- | Add missing identifiers.
identify : String -> R.Tree (Maybe Node) -> R.Tree Node
identify dummyVal tree =
  let
    newId1 = case findMaxID tree of
       Nothing -> 1
       Just ix -> ix + 1
    update newId nodeMay =
      case nodeMay of
        Nothing -> (newId+1, Node {nodeId=newId, nodeVal=dummyVal, nodeTyp=Nothing, nodeComment=""})
        Just x  -> (newId, x)
  in
    Tuple.second <| R.mapAccum update newId1 tree


-- | Completely re-identify the given tree.
reID : R.Tree Node -> R.Tree Node
reID =
    let
        update newId x =
            ( newId + 1
            , Lens.set nodeId newId x )
    in
        Tuple.second << R.mapAccum update 0


---------------------------------------------------
-- Change the type of the selected node
---------------------------------------------------


-- -- | Change the type of the main selected nodes in a given window.
-- changeTypeSel : Focus -> Model -> Model
-- changeTypeSel = changeWith changeType
--
--
-- -- | Change the type of a given node.
-- changeType : NodeId -> R.Tree Node -> R.Tree Node
-- changeType =
--   let
--     shiftTyp x = case x of
--       Nothing -> Just <| NodeEvent Anno.eventDefault
--       Just (NodeEvent _) -> Just <| NodeSignal Anno.signalDefault
--       Just (NodeSignal _) -> Just <| NodeTimex Anno.timexDefault
--       Just (NodeTimex _) -> Nothing
--   in
--     changeTypeWith shiftTyp


-- | Change the type of a given node.
-- changeTypeWith : NodeId -> R.Tree Node -> R.Tree Node
changeTypeWith shiftTyp id =
  let
    update x =
      if id == Lens.get nodeId x && not (isLeaf x)
      then shift x
      else x
    shift node = case node of
      Leaf r -> node
      Node r -> Node <| {r | nodeTyp = shiftTyp r.nodeTyp}
  in
    R.map update


-- | An abstraction over `changeTypeSel` and similar functions.
changeWith
    : (NodeId -> R.Tree Node -> R.Tree Node)
    -> Focus
    -> Model
    -> Model
changeWith changeFun focus model =
  let
    win = selectWin focus model
    treeId = getReprId focus win.tree model
    tree = getTree focus treeId model
  in
    case win.selMain of
      Nothing -> model
      Just id -> updateTree focus treeId (\_ -> changeFun id tree) model


---------------------------------------------------
-- Create an entity
---------------------------------------------------


mkEntitySel : AnnoCfg.Entity -> Focus -> Model -> Model
mkEntitySel entCfg =
  let
    add x =
      case x of
        Just ent -> ent.name /= entCfg.name
        _ -> True
    mkTyp x =
      if add x
      then Just <| Anno.defaultEntity entCfg
      else Nothing
  in
    changeWith (changeTypeWith mkTyp)


-- ---------------------------------------------------
-- -- Create signal
-- ---------------------------------------------------
--
--
-- mkSignalSel : Focus -> Model -> Model
-- mkSignalSel = changeWith mkSignal
--
--
-- -- | Mark a signal.
-- mkSignal : NodeId -> R.Tree Node -> R.Tree Node
-- mkSignal =
--   let
--     mkTyp x = case x of
--       Just (NodeSignal _) -> Nothing
--       _ -> Just <| NodeSignal Anno.signalDefault
--   in
--     changeTypeWith mkTyp
--
--
-- ---------------------------------------------------
-- -- Create event
-- ---------------------------------------------------
--
--
-- mkEventSel : Focus -> Model -> Model
-- mkEventSel =
--   let
--     mkTyp x = case x of
--       Just (NodeEvent _) -> Nothing
--       _ -> Just <| NodeEvent Anno.eventDefault
--   in
--     changeWith (changeTypeWith mkTyp)
--
--
-- ---------------------------------------------------
-- -- Create timex
-- ---------------------------------------------------
--
--
-- mkTimexSel : Focus -> Model -> Model
-- mkTimexSel =
--   let
--     mkTyp x = case x of
--       Just (NodeTimex _) -> Nothing
--       _ -> Just <| NodeTimex Anno.timexDefault
--   in
--     changeWith (changeTypeWith mkTyp)


---------------------------------------------------
-- Concat words
---------------------------------------------------


-- | Concatenate selected words (if contiguous) and destroy the tree (side-effet...).
concatWords : Model -> Model
concatWords model =
    let
        process idSet sent tree =
            (\toks -> (L.map Tuple.first toks, treeFromSent toks)) <|
            concatSelectedToks (leafPosSet tree idSet) <|
            syncTreeWithSent 0 sent tree
    in
        procSelWithSent process model.focus model


-- | Concatenate user-selected tokens.
concatSelectedToks
     : S.Set Int -- ^ Position IDs
    -> List (Token, Maybe String)
    -> List (Token, Maybe String)
concatSelectedToks posSet =
    let
        go pos acc xs =
            case xs of
                hd :: tl ->
                    if S.member pos posSet then
                        go (pos+1) (hd :: acc) tl
                    else
                        reveal acc ++ [hd] ++ go (pos+1) [] tl
                [] -> reveal acc
        reveal toks =
            if L.isEmpty toks
            then []
            else
                let (xs, ys) = L.unzip <| L.reverse toks
                    tok = concatToks xs
                    str = Str.join " " <| Util.catMaybes ys
                in  [(tok, Just str)]
    in
        go 0 []


-- | Retrieve the set of selected leaf positions.
leafPosSet : R.Tree Node -> S.Set NodeId -> S.Set Int
leafPosSet tree idSet =
    let
        getSelected node =
            case node of
                Node _ -> Nothing
                Leaf r ->
                    if S.member r.nodeId idSet
                    then Just r.leafPos
                    else Nothing
    in
        R.flatten tree |>
        L.map getSelected |>
        Util.catMaybes |>
        S.fromList


type alias Orth = String
type alias Pos = String


-- | Synchronize the tree with the corresponding sentence.
syncTreeWithSent
    : Int   -- ^ Token ID corresponding to the sentence's first token.
    -> Sent
    -> R.Tree Node -> List (Token, Maybe Orth)
syncTreeWithSent firstTokId sent tree =
    let onSecond f (x, y) = (x, f y)
        simplify = onSecond <| Maybe.map Tuple.first
    in  L.map simplify <| syncTreeWithSentPos firstTokId sent tree
--     let
--         go pos toks leaves =
--             case (toks, leaves) of
--                 (tok :: toksRest, leaf :: leavesRest) ->
--                     if leaf.leafPos == pos
--                     then (tok, Just leaf.nodeVal) :: go (pos+1) toksRest leavesRest
--                     else (tok, Nothing) :: go (pos+1) toksRest leaves
--                 (tok :: toksRest, []) ->
--                     (tok, Nothing) :: go (pos+1) toksRest []
--                 ([], _) -> []
--     in
--         go 0 sent (getWords tree)


-- | Synchronize the tree with the corresponding sentence.
-- The result contains information about the POS tags.
syncTreeWithSentPos
    : Int   -- ^ Token ID corresponding to the sentence's first token.
    -> Sent
    -> R.Tree Node -> List (Token, Maybe (Orth, Pos))
syncTreeWithSentPos firstTokId sent tree =
    let
        go pos toks leaves =
            case (toks, leaves) of
                (tok :: toksRest, (leaf, posTag) :: leavesRest) ->
                    if leaf.leafPos == pos
                    then (tok, Just (leaf.nodeVal, posTag)) :: go (pos+1) toksRest leavesRest
                    else (tok, Nothing) :: go (pos+1) toksRest leaves
                (tok :: toksRest, []) ->
                    (tok, Nothing) :: go (pos+1) toksRest []
                ([], _) -> []
    in
        go firstTokId sent (getWordsPos tree)


-- | Make a (completely flat) tree from the given list of strings.
treeFromSent : List (Token, Maybe String) -> R.Tree Node
treeFromSent = treeFromSent_ << determinePositions


-- | Make a (completely flat) tree from the given list of strings. The second
-- elements in the input list represent positions in the sentence (and thus
-- indicate the corresponding tokens).
treeFromSent_ : List (String, Int) -> R.Tree Node
treeFromSent_ xs =
    let
        root = Node {nodeId=0, nodeVal="ROOT", nodeTyp=Nothing, nodeComment=""}
        sent = Node {nodeId=1, nodeVal="SENT", nodeTyp=Nothing, nodeComment=""}
        leaves = L.map mkLeaf
                 <| L.map2 (,) xs
                 <| L.range 1 (L.length xs)
        mkLeaf ((word, leafPos), nodeId) = R.leaf <|
            Leaf {nodeId=nodeId+1, nodeVal=word, leafPos=leafPos, nodeComment=""}
    in
        R.Node root [R.Node sent leaves]



-- | Determine positions of the Just strings in the list.
-- Discard the Nothing values.
determinePositions : List (Token, Maybe String) -> List (String, Int)
determinePositions =
    let
        upd pos (tok, mayStr) =
            case mayStr of
                Nothing  -> (pos + 1, Nothing)
                Just str -> (pos + 1, Just (str, pos))
    in
        Util.catMaybes << Tuple.second << Util.mapAccumL upd 0


---------------------------------------------------
-- Generalized `synTreeWithSent`
---------------------------------------------------


-- | Syncronize the forest with the corresponding sentence.
syncForestWithSent : Sent -> R.Forest Node -> List (List (Token, Maybe Orth))
syncForestWithSent sent forest =
    let onSecond f (x, y) = (x, f y)
        simplify = onSecond <| Maybe.map Tuple.first
    in  L.map (L.map simplify) <| syncForestWithSentPos sent forest
--     let
--         tail xs =
--             case L.tail xs of
--                 Nothing -> []
--                 Just ys -> ys
--         minList = tail <| L.map minimalPosition forest
--         sentList = segmentSentence minList sent
--         sync (subSent, subTree) = syncTreeWithSent subSent subTree
--     in
--         L.map sync (L.map2 (,) sentList forest)


-- | Syncronize the forest with the corresponding sentence.
syncForestWithSentPos : Sent -> R.Forest Node -> List (List (Token, Maybe (Orth, Pos)))
syncForestWithSentPos sent forest =
    let
        tail xs =
            case L.tail xs of
                Nothing -> []
                Just ys -> ys
        minList = tail <| L.map minimalPosition forest
        sentList = segmentSentence minList sent
        sync (firstTokId, subSent, subTree) = syncTreeWithSentPos firstTokId subSent subTree
        zip i xs ys =
            case (xs, ys) of
                (xs1 :: xsRest, ys1 :: ysRest) ->
                    (i, xs1, ys1) :: zip (i + L.length xs1) xsRest ysRest
                _ -> []
    in
        L.map sync (zip 0 sentList forest)


-- | The smallest leaf position (token ID) in the given tree.
minimalPosition : R.Tree Node -> Int
minimalPosition =
    let takeMin xs =
            case L.minimum xs of
                Nothing -> Debug.crash "minimalPosition: empty tree"
                Just x -> x
    in  takeMin << L.map .leafPos << getWords


-- | Segment the given sentence on the given positions.  The input list should be ascending.
-- TODO: make sure it works in corner cases. (e.g. when splitList == []).
segmentSentence : List Int -> Sent -> List Sent
segmentSentence splitList sent =
    case splitList of
        [] -> [sent]
        split :: splitRest ->
            let (sentLeft, sentRight) = Util.splitAt split sent
                newSplitRest = L.map (\i -> i - split) splitRest
            in  sentLeft :: segmentSentence newSplitRest sentRight


---------------------------------------------------
-- Tree splitting
---------------------------------------------------


-- | Split the current tree into several sentences.
splitTree : Model -> Model
splitTree model =

    let

        -- Group words into segments
        go idSet acc xs =
            case xs of
                hd :: tl ->
                    if S.member hd.nodeId idSet then
                        L.reverse acc :: go idSet [hd] tl
                    else
                        go idSet (hd :: acc) tl
                [] -> [L.reverse acc]

        mkSent xs =
            let
                sent = Node {nodeId=0, nodeVal="SENT", nodeTyp=Nothing, nodeComment=""}
                leaves = L.map (R.leaf << Leaf) xs
            in
                R.Node sent leaves

        mkTree xss =
            let
                root = Node {nodeId=0, nodeVal="ROOT", nodeTyp=Nothing, nodeComment=""}
                subTrees = L.map mkSent xss
            in
                R.Node root subTrees

        process idSet tree =
            reID
            <| mkTree
            <| L.filter (not << L.isEmpty)
            <| go idSet [] (getWords tree)

    in

        procSel process model.focus model


---------------------------------------------------
-- Join sentences
---------------------------------------------------


-- | Join two partitions.
--
-- NOTE: as an effect of this operation, one of its arguments becomes invalid
-- (is no longer a partition representative).
join : Focus -> PartId -> PartId -> Model -> Model
join win tid1 tid2 model =
    if tid1 == tid2
    then model
    else joinIndeed win tid1 tid2 model


-- | Internal join function which does not check that the two given ids can
-- really be joined.
joinIndeed : Focus -> PartId -> PartId -> Model -> Model
joinIndeed win tid1 tid2 model =
    let
        newRepr = if tid1 < tid2 then tid1 else tid2
        oldRepr = if tid1 < tid2 then tid2 else tid1
        newPart = S.union
                  (Maybe.withDefault S.empty <| getPart win newRepr model)
                  (Maybe.withDefault S.empty <| getPart win oldRepr model)
        newTree = joinTrees
                  (getTree win newRepr model)
                  (getTree win oldRepr model)
        addPOS mod = R.map <| Lens.update leafPos (\k->k+mod)
        joinTrees t1 t2Init =
            let root = Node {nodeId=0, nodeVal="ROOT", nodeTyp=Nothing, nodeComment=""}
                shift = L.length <| getSent win newRepr model
                t2 = addPOS shift t2Init
            -- in  reID <| rePOS <| R.Node root (R.subTrees t1 ++ R.subTrees t2)
            in  reID <| R.Node root (R.subTrees t1 ++ R.subTrees t2)
    in
        setPart win newRepr (Just newPart) <| setRepr win oldRepr newRepr <|
        -- setPart oldRepr newPart <|
        removePart win oldRepr <|
        -- NOTE: the only reason to first remove the `newRepr` tree and then to
        -- update it is to get the corresponding links deleted.
        setTree win newRepr (Just newTree) <| removeTree win newRepr <|
        -- updateTree newRepr (\_ -> newTree) <|
        removeTree win oldRepr <|
        model


-- | Remove partition under the given partition ID.
removePart : Focus -> PartId -> Model -> Model
removePart win treeId = setPart win treeId Nothing


-- | Set partition for the given tree ID.
setPart : Focus -> PartId -> Maybe (S.Set TreeIdBare) -> Model -> Model
setPart win treeId newPartMay model =
    let
        oldPart = getPart win treeId model
    in
        setPart_ win treeId newPartMay model
            |> saveModif (getFileId win model)
               ( PartModif
                 { treeId = treeId
                 , restorePart = oldPart }
               )


-- | Set partition for the given tree ID.
setPart_ : Focus -> PartId -> Maybe (S.Set TreeIdBare) -> Model -> Model
setPart_ win treeId newPartMay model =
    setPartAlt_ (getFileId win model) treeId newPartMay model
--     let
--         newPartMap partMap =
--             case newPartMay of
--                 Nothing -> D.remove treeId partMap
--                 Just newPart -> D.insert treeId newPart partMap
--         newModel = Lens.update (fileLens win => partMap) newPartMap model
--     in
--         newModel


-- | Set partition for the given tree ID.
setPartAlt_ : FileId -> PartId -> Maybe (S.Set TreeIdBare) -> Model -> Model
setPartAlt_ fileId treeId newPartMay model =
    let
        newPartMap partMap =
            case newPartMay of
                Nothing -> D.remove treeId partMap
                Just newPart -> D.insert treeId newPart partMap
        newModel = Lens.update (fileLensAlt fileId => partMap) newPartMap model
    in
        newModel


getPartAlt : FileId -> PartId -> Model -> Maybe (S.Set TreeIdBare)
getPartAlt fileId treeId =
    D.get treeId << Lens.get (fileLensAlt fileId => partMap)


getPart : Focus -> PartId -> Model -> Maybe (S.Set TreeIdBare)
getPart win treeId model =
    getPartAlt (getFileId win model) treeId model
    -- D.get treeId << Lens.get (fileLens win => partMap)


-- | Set representative for the given tree ID.
setRepr : Focus -> TreeIdBare -> TreeIdBare -> Model -> Model
setRepr win treeId newRepr model =
    let
        oldRepr = getRepr win treeId model
        newModel = Lens.update (fileLens win => reprMap) (D.insert treeId newRepr) model
    in
        newModel
            |> saveModif (getFileId win model)
               ( ReprModif
                 { treeId = treeId
                 , restoreRepr = oldRepr }
               )


getRepr : Focus -> TreeIdBare -> Model -> TreeIdBare
getRepr win treeId model =
    getReprAlt (getFileId win model) treeId model


getReprAlt : FileId -> TreeIdBare -> Model -> TreeIdBare
getReprAlt fileId treeId model =
    let
        file = Lens.get (fileLensAlt fileId) model
    in
        case D.get treeId file.reprMap of
            Nothing -> Debug.crash "getReprAlt: no representative for a given ID"
            Just st -> st


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
    treeId = getReprId focus win.tree model
    inTree = getTree focus treeId model
  in
    case (fromMay, toMay) of
      (Just from, Just to) ->
        case attach from to inTree of
          Just newTree ->
            updateTree focus treeId (\_ -> newTree) model
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
      |> Maybe.andThen (Util.check wellFormed)
      |> Maybe.map (sortTree to))
      -- |> Maybe.andThen (Util.check wellFormed))


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
    treeId = getReprId focus win.tree model
    inTree = getTree focus treeId model
    -- left = not model.ctrl
  in
    nodeMay
      |> Maybe.andThen (\nodeId -> swap left nodeId inTree
      |> Maybe.map (\newTree -> updateTree focus treeId (\_ -> newTree) model))
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
      |> Util.check wellFormed


---------------------------------------------------
-- Popups
---------------------------------------------------


-- | Change the value of the split in the popup window.
setFilesPopup : List FileId -> Model -> Model
setFilesPopup fileIds model =
    case model.popup of
        Just (Popup.Files _) ->
            {model | popup = Just (Popup.Files (Just fileIds))}
        _ -> model


-- | Change the value of the split in the popup window.
changeSplit : Int -> Model -> Model
changeSplit k model =
    case model.popup of
        Just (Popup.Split spl) ->
            let newSpl = {spl | split = k}
            in  {model | popup = Just (Popup.Split newSpl)}
        _ -> model


-- | Perform the split on the selected terminal node.
performSplit : Int -> Model -> Model
performSplit splitPlace model =
    let
        focus = model.focus
        win = selectWin focus model
        partId = getReprId focus win.tree model
        updTree theID tokID tree =
            let
                newId1 = case findMaxID (R.map Just tree) of
                   Nothing -> 1
                   Just ix -> ix + 1
                single leaf =
                    let
                        new =
                            if leaf.leafPos > tokID
                            then {leaf | leafPos = leaf.leafPos + 1}
                            else leaf
                    in
                        [R.Node (Leaf new) []]
                duplicate leaf =
                    let
                        nodeVal = (getToken leaf.leafPos focus partId model).orth
                        leftVal = String.left splitPlace nodeVal
                        rightVal = String.dropLeft splitPlace nodeVal
                        left  = {leaf | nodeVal = String.trim leftVal}
                        right = {leaf
                                    | nodeVal = String.trim rightVal
                                    , nodeId = newId1
                                    , leafPos = leaf.leafPos + 1
                                }
                    in
                        [ R.Node (Leaf left) []
                        , R.Node (Leaf right) [] ]
                go (R.Node x ts) = case x of
                    Node r -> [R.Node x (L.concatMap go ts)]
                    Leaf r ->
                        if r.nodeId == theID
                        then duplicate r
                        else single r
            in
                 case go tree of
                     [t] -> t
                     _   -> tree
    in
        case win.selMain of
            Nothing -> model
            Just id ->
                let tokID = Lens.get leafPos <| getNode id focus model
                in  updateTree focus partId (updTree id tokID) model
                    |> performTokenSplit tokID splitPlace focus partId


-- | Split the given token in the sentence corresponding to the given partition.
performTokenSplit
    : Int  -- ^ The token ID (position)
    -> Int -- ^ Split place
    -> Focus
    -> PartId
    -> Model
    -> Model
performTokenSplit tokID splitPlace win partId model =
    let
        oldSent = getSent win partId model
        newSent = splitToken tokID splitPlace oldSent
    in
        setSent win partId newSent model
            |> Maybe.withDefault model


-- | Split the given token in the sentence.
splitToken
    : Int  -- ^ The token ID (position)
    -> Int -- ^ Split place
    -> Sent
    -> Sent
splitToken tokID splitPlace toks =
    let
        leftToks  = L.take tokID toks
        rightToks = L.drop tokID toks
        splitTok tok =
            let
                leftOrth = String.left splitPlace tok.orth
                rightOrth = String.dropLeft splitPlace tok.orth
                left =
                    { orth = String.trim leftOrth
                    , afterSpace = tok.afterSpace }
                right =
                    { orth = String.trim rightOrth
                    , afterSpace =
                        String.endsWith " " leftOrth ||
                        String.startsWith " " rightOrth
                    }
            in
                [left, right]
    in
        case rightToks of
            [] -> toks
            hd :: tl -> leftToks ++ splitTok hd ++ tl


---------------------------------------------------
-- Restore token
---------------------------------------------------


-- -- | Restore the given token (provided that it can even be restored).
-- restoreToken
--     : PartId
--     -> Int     -- ^ Token ID of the token to restore
--     -> Model
--     -> Model
-- restoreToken partID tokID model =
--     if not <| M.isDummyTree <| getTree treeId model
--     then doRestoreToken treeId tokID model
--     else
--         let
--             tok = getToken 0 partID model
--         in


-- | Restore the given token (provided that it can even be restored).
-- Does not handle the dummy case.
restoreToken
    : Focus
    -> PartId
    -> Int     -- ^ Token ID of the token to restore
    -> Model
    -> Model
restoreToken win partID tokID model =
    let
        tree = getTree win partID model
        tok = getToken tokID win partID model
        visible = visiblePositions tree
        isVisible i = S.member i visible
        onRight =
            findLeftClosestToken tokID tree
                |> Maybe.andThen
                   (\sisterTokID -> addSister True sisterTokID (tokID, tok.orth) tree)
        onLeft =
            findRightClosestToken tokID tree
                |> Maybe.andThen
                   (\sisterTokID -> addSister False sisterTokID (tokID, tok.orth) tree)
        newModel =
            Util.guard (not <| isVisible tokID)
                |> Maybe.andThen (\_ -> Util.mappend onRight onLeft)
                |> Maybe.map (\newTree -> updateTree win partID (\_ -> newTree) model)
                |> Maybe.withDefault model
    in
        newModel


-- | Find in the tree the closest token on the left from the given position. If
-- a leaf with the given position exists in the tree, its position is retrieved.
findLeftClosestToken
    :  Int           -- ^ Token ID
    -> R.Tree Node   -- ^ Tree
    -> Maybe Int     -- ^ The resulting token ID
findLeftClosestToken tokID =
    let
        f leaf =
            if leaf.leafPos <= tokID
            then Just leaf.leafPos
            else Nothing
    in
        L.foldl Util.mappend Nothing << L.map f << getWords


-- | Find in the tree the closest token on the right from the given position. If
-- a leaf with the given position exists in the tree, its position is retrieved.
findRightClosestToken
    :  Int           -- ^ Token ID
    -> R.Tree Node   -- ^ Tree
    -> Maybe Int     -- ^ The resulting token ID
findRightClosestToken tokID =
   let
       f leaf =
           if leaf.leafPos >= tokID
           then Just leaf.leafPos
           else Nothing
   in
       L.foldr Util.mappend Nothing << L.map f << getWords


-- | Add new terminal node just on the right of the terminal with the given token ID.
addSister
    :  Bool          -- ^ True if on the right, otherwise on the left
    -> Int           -- ^ Token ID of the token which should be blessed with the sister
    -> (Int, String) -- ^ Token ID of the new terminal node and its label
    -> R.Tree Node   -- ^ Tree
    -> Maybe (R.Tree Node)  -- ^ The resulting tree
addSister onRight tokID (newTokID, label) tree =
    let
        newId =
            case findMaxID (R.map Just tree) of
                Nothing -> 1
                Just ix -> ix + 1
        addIt old =
            let
                new =
                    { nodeVal = label
                    , nodeId = newId
                    , leafPos = newTokID
                    , nodeComment = "" }
                process =
                    if onRight
                    then identity
                    else L.reverse
            in
                process
                [ R.Node (Leaf old) []
                , R.Node (Leaf new) [] ]
        go (R.Node x ts) =
            case x of
                Node r -> [R.Node x (L.concatMap go ts)]
                Leaf r ->
                    if r.leafPos == tokID
                    then addIt r
                    else [R.Node (Leaf r) []]
    in
        case go tree of
            [t] -> Just t
            _ -> Nothing


-- | Calculate the set of visible positions in the sentence.
-- Takes into account the dummy case.
visiblePositions : R.Tree Node -> S.Set Int
visiblePositions tree =
    if isDummyTree tree
    then S.empty
    else S.fromList << L.map .leafPos << getWords <| tree


---------------------------------------------------
-- Dummy
---------------------------------------------------


-- | Replace the tree assigned to the given partition by the dummy tree.
dumify : Focus -> PartId -> Model -> Model
dumify win partId model =
    let
        oldSent = getSent win partId model
        newSent = [concatToks oldSent]
    in
        setTree win partId (Just dummyTree) model
        |> setSent win partId newSent
        |> Maybe.withDefault model


-- | A dummy tree, corresponding to an empty sentence.
dummyTree : R.Tree Node
dummyTree =
    let
        root = Node {nodeId=0, nodeVal="ROOT", nodeTyp=Nothing, nodeComment=""}
        leaf = Leaf {nodeId=1, nodeVal="", leafPos=0, nodeComment=""}
    in
        R.Node root [R.Node leaf []]


-- | Check whether the given tree is the dummy tree.
isDummyTree : R.Tree Node -> Bool
isDummyTree tree =
    case getWords tree of
        [leaf] -> leaf.nodeVal == ""
        _ -> False


---------------------------------------------------
-- Goto
---------------------------------------------------


-- -- | Go to a given address in the focused window.
-- goto : C.Addr -> M.Model -> M.Model
-- goto addr model =
--     let
--         focus = model.focus
--         win = selectWin focus model
--         nodeMay = win.selMain
--         inTree = getTree win.tree model
--     in


---------------------------------------------------
-- Utils
---------------------------------------------------


-- | Flip the first two arguments.
flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x


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


fileId : Lens.Focus { record | fileId : a } a
fileId = Lens.create
  .fileId
  (\fn model -> {model | fileId = fn model.fileId})


file : Lens.Focus { record | file : a } a
file = Lens.create
  .file
  (\fn model -> {model | file = fn model.file})


-- mainFile : Lens.Focus { record | mainFile : a } a
-- mainFile = Lens.create
--   .mainFile
--   (\fn model -> {model | mainFile = fn model.mainFile})
--
--
-- cmpFile : Lens.Focus { record | cmpFile : Maybe a, mainFile : a } a
-- cmpFile =
--     let
--         get model =
--             case model.cmpFile of
--                 Nothing -> model.mainFile
--                 Just fi -> fi
--         update fn model =
--             case model.cmpFile of
--                 Nothing -> {model | mainFile = fn model.mainFile}
--                 Just fi -> {model | cmpFile = Just (fn fi)}
--     in
--         Lens.create get update
--
--
-- -- | Generic file lens whose behavior depends on the window in focus.
-- fileLens : Focus -> Lens.Focus { record | cmpFile : Maybe a, mainFile : a } a
-- fileLens win =
--     case win of
--         Top -> mainFile
--         Bot -> cmpFile


fileInfoLensAlt : FileId -> Lens.Focus Model FileInfo
fileInfoLensAlt fid =
    let
        get model =
            case Util.find (((==) fid) << Tuple.first) model.fileList of
                Nothing -> Debug.crash "Model.fileLens.get: unknown fileID"
                Just (_, info) -> info
        update fn model =
            let
                go xs = case xs of
                    [] -> Debug.crash "Model.fileLens.update: unknown fileID"
                    ((fileId, info) :: rest) ->
                        if fileId == fid
                        then (fileId, fn info) :: rest
                        else (fileId, info) :: go rest
            in
                {model | fileList = go model.fileList}
    in
        Lens.create get update


fileInfoLens : Focus -> Lens.Focus Model FileInfo
fileInfoLens win =
    let
        get model = Lens.get (fileInfoLensAlt <| getFileId win model) model
        update fn model =
            Lens.update (fileInfoLensAlt (getFileId win model)) fn model
    in
        Lens.create get update


fileLens : Focus -> Lens.Focus Model File
fileLens win =
    let
        get = Lens.get (fileInfoLens win => file)
        update = Lens.update (fileInfoLens win => file)
    in
        Lens.create get update


fileLensAlt : FileId -> Lens.Focus Model File
fileLensAlt fileId =
    let
        get = Lens.get (fileInfoLensAlt fileId => file)
        update = Lens.update (fileInfoLensAlt fileId => file)
    in
        Lens.create get update


getFileId : Focus -> Model -> FileId
getFileId win =
    Lens.get (workspaceLens win => fileId)
--     case win of
--         Top -> model.mainFileId
--         Bot ->
--             case model.cmpFileId of
--                 Nothing -> model.mainFileId
--                 Just id -> id


treeMap : Lens.Focus { record | treeMap : a } a
treeMap = Lens.create
  .treeMap
  (\fn model -> {model | treeMap = fn model.treeMap})


sentMap : Lens.Focus { record | sentMap : a } a
sentMap = Lens.create
  .sentMap
  (\fn model -> {model | sentMap = fn model.sentMap})


partMap : Lens.Focus { record | partMap : a } a
partMap = Lens.create
  .partMap
  (\fn model -> {model | partMap = fn model.partMap})


reprMap : Lens.Focus { record | reprMap : a } a
reprMap = Lens.create
  .reprMap
  (\fn model -> {model | reprMap = fn model.reprMap})


linkMap : Lens.Focus { record | linkMap : a } a
linkMap = Lens.create
  .linkMap
  (\fn model -> {model | linkMap = fn model.linkMap})


turns : Lens.Focus { record | turns : a } a
turns = Lens.create
  .turns
  (\fn model -> {model | turns = fn model.turns})


trees : Lens.Focus { record | trees : a } a
trees = Lens.create
  .trees
  (\fn model -> {model | trees = fn model.trees})


-- | Focus on the window in given focus (well, yes, the names are not
-- perfect...).
windowLens : Focus -> Lens.Focus Model Window
windowLens win =
    let
        get model =
            let info = Lens.get (fileInfoLens win) model in
            case win of
                Top -> info.top
                Bot -> info.bot
        updateInfo fn info =
            case win of
                Top -> {info | top = fn info.top}
                Bot -> {info | bot = fn info.bot}
        update fn = Lens.update (fileInfoLens win) (updateInfo fn)
    in
        Lens.create get update


-- -- | Focus on the given workspace.
-- workspaceLens : Focus -> Lens.Focus Model Workspace
-- workspaceLens focus =
--   case focus of
--     Top -> top
--     Bot -> bot


workspaceLens : Focus -> Lens.Focus { record | bot : a, top : a } a
workspaceLens focus =
  case focus of
    Top -> top
    Bot -> bot


selMain : Lens.Focus { record | selMain : a } a
selMain = Lens.create
  .selMain
  (\fn model -> {model | selMain = fn model.selMain})


dim : Lens.Focus { record | dim : a } a
dim = Lens.create
  .dim
  (\fn model -> {model | dim = fn model.dim})


-- links : Lens.Focus { record | links : a } a
-- links = Lens.create
--   .links
--   (\fn model -> {model | links = fn model.links})


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


-- | NOTE: does nothing reasonable for internal nodes.
leafPos : Lens.Focus Node Int
leafPos =
  let
    get node = case node of
      Node r -> 0
      Leaf r -> r.leafPos
    update f node = case node of
      Node r -> Node r
      Leaf r -> Leaf {r | leafPos = f r.leafPos}
  in
    Lens.create get update


nodeTyp : Lens.Focus Node (Maybe NodeAnnoTyp)
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


nodeComment : Lens.Focus Node String
nodeComment =
  let
    get node = case node of
      Node r -> r.nodeComment
      Leaf r -> r.nodeComment
    update f node = case node of
      Node r -> Node {r | nodeComment = f r.nodeComment}
      Leaf r -> Leaf {r | nodeComment = f r.nodeComment}
  in
    Lens.create get update


-- nodeEvent : Lens.Focus NodeAnnoTyp Anno.Event
-- nodeEvent =
--   let
--     getErr = "nodeEvent.lens: cannot get"
--     get typ = case typ of
--       NodeEvent event -> event
--       _ -> Debug.crash getErr
--     update f typ = case typ of
--       NodeEvent event -> NodeEvent (f event)
--       _ -> typ
--   in
--     Lens.create get update
--
--
-- nodeSignal : Lens.Focus NodeAnnoTyp Anno.Signal
-- nodeSignal =
--   let
--     getErr = "nodeSignal.lens: cannot get"
--     get typ = case typ of
--       NodeSignal event -> event
--       _ -> Debug.crash getErr
--     update f typ = case typ of
--       NodeSignal event -> NodeSignal (f event)
--       _ -> typ
--   in
--     Lens.create get update
--
--
-- nodeTimex : Lens.Focus NodeAnnoTyp Anno.Timex
-- nodeTimex =
--   let
--     getErr = "nodeTimex.lens: cannot get"
--     get typ = case typ of
--       NodeTimex timex -> timex
--       _ -> Debug.crash getErr
--     update f typ = case typ of
--       NodeTimex timex -> NodeTimex (f timex)
--       _ -> typ
--   in
--     Lens.create get update


-- ----------------------------
-- -- Event-related lenses
-- ----------------------------
--
--
-- eventClass : Lens.Focus Anno.Event Anno.EventClass
-- eventClass =
--   let
--     get (Anno.Event r) = r.evClass
--     update f (Anno.Event r) = Anno.Event {r | evClass = f r.evClass}
--   in
--     Lens.create get update
--
--
-- eventType : Lens.Focus Anno.Event Anno.EventType
-- eventType =
--   let
--     get (Anno.Event r) = r.evType
--     update f (Anno.Event r) = Anno.Event {r | evType = f r.evType}
--   in
--     Lens.create get update
--
--
-- eventInquisit : Lens.Focus Anno.Event Anno.EventInquisit
-- eventInquisit =
--   let
--     get (Anno.Event r) = r.evInquisit
--     update f (Anno.Event r) = Anno.Event {r | evInquisit = f r.evInquisit}
--   in
--     Lens.create get update
--
--
-- eventTime : Lens.Focus Anno.Event (Maybe Anno.EventTime)
-- eventTime =
--   let
--     get (Anno.Event r) = r.evTime
--     update f (Anno.Event r) = Anno.Event {r | evTime = f r.evTime}
--   in
--     Lens.create get update
--
--
-- eventAspect : Lens.Focus Anno.Event (Maybe Anno.EventAspect)
-- eventAspect =
--   let
--     get (Anno.Event r) = r.evAspect
--     update f (Anno.Event r) = Anno.Event {r | evAspect = f r.evAspect}
--   in
--     Lens.create get update
--
--
-- eventPolarity : Lens.Focus Anno.Event Anno.EventPolarity
-- eventPolarity =
--   let
--     get (Anno.Event r) = r.evPolarity
--     update f (Anno.Event r) = Anno.Event {r | evPolarity = f r.evPolarity}
--   in
--     Lens.create get update
--
--
-- eventMood : Lens.Focus Anno.Event (Maybe Anno.EventMood)
-- eventMood =
--   let
--     get (Anno.Event r) = r.evMood
--     update f (Anno.Event r) = Anno.Event {r | evMood = f r.evMood}
--   in
--     Lens.create get update
--
--
-- eventModality : Lens.Focus Anno.Event (Maybe Anno.EventModality)
-- eventModality =
--   let
--     get (Anno.Event r) = r.evModality
--     update f (Anno.Event r) = Anno.Event {r | evModality = f r.evModality}
--   in
--     Lens.create get update
--
--
-- eventCardinality : Lens.Focus Anno.Event String
-- eventCardinality =
--   let
--     get (Anno.Event r) = r.evCardinality
--     update f (Anno.Event r) = Anno.Event {r | evCardinality = f r.evCardinality}
--   in
--     Lens.create get update
--
--
-- eventMod : Lens.Focus Anno.Event (Maybe Anno.EventMod)
-- eventMod =
--   let
--     get (Anno.Event r) = r.evMod
--     update f (Anno.Event r) = Anno.Event {r | evMod = f r.evMod}
--   in
--     Lens.create get update
--
--
-- eventPred : Lens.Focus Anno.Event String
-- eventPred =
--   let
--     get (Anno.Event r) = r.evPred
--     update f (Anno.Event r) = Anno.Event {r | evPred = f r.evPred}
--   in
--     Lens.create get update


----------------------------
-- Signal-related lenses
----------------------------


-- signalType : Lens.Focus Anno.Signal Anno.SignalType
-- signalType =
--   let
--     get (Anno.Signal r) = r.siType
--     update f (Anno.Signal r) = Anno.Signal {r | siType = f r.siType}
--   in
--     Lens.create get update


-- ----------------------------
-- -- Timex-related lenses
-- ----------------------------
--
--
-- timexCalendar : Lens.Focus Anno.Timex Anno.TimexCalendar
-- timexCalendar =
--   let
--     get (Anno.Timex r) = r.tiCalendar
--     update f (Anno.Timex r) = Anno.Timex {r | tiCalendar = f r.tiCalendar}
--   in
--     Lens.create get update
--
--
-- timexType : Lens.Focus Anno.Timex Anno.TimexType
-- timexType =
--   let
--     get (Anno.Timex r) = r.tiType
--     update f (Anno.Timex r) = Anno.Timex {r | tiType = f r.tiType}
--   in
--     Lens.create get update
--
--
-- timexFunctionInDocument : Lens.Focus Anno.Timex (Maybe Anno.TimexFunctionInDocument)
-- timexFunctionInDocument =
--   let
--     get (Anno.Timex r) = r.tiFunctionInDocument
--     update f (Anno.Timex r) = Anno.Timex {r | tiFunctionInDocument = f r.tiFunctionInDocument}
--   in
--     Lens.create get update
--
--
-- timexPred : Lens.Focus Anno.Timex String
-- timexPred =
--   let
--     get (Anno.Timex r) = r.tiPred
--     update f (Anno.Timex r) = Anno.Timex {r | tiPred = f r.tiPred}
--   in
--     Lens.create get update
--
--
-- timexTemporalFunction : Lens.Focus Anno.Timex (Maybe Anno.TimexTemporalFunction)
-- timexTemporalFunction =
--   let
--     get (Anno.Timex r) = r.tiTemporalFunction
--     update f (Anno.Timex r) = Anno.Timex {r | tiTemporalFunction = f r.tiTemporalFunction}
--   in
--     Lens.create get update
--
--
-- timexLingValue : Lens.Focus Anno.Timex String
-- timexLingValue =
--   let
--     get (Anno.Timex r) = r.tiLingValue
--     update f (Anno.Timex r) = Anno.Timex {r | tiLingValue = f r.tiLingValue}
--   in
--     Lens.create get update
--
--
-- timexValue : Lens.Focus Anno.Timex String
-- timexValue =
--   let
--     get (Anno.Timex r) = r.tiValue
--     update f (Anno.Timex r) = Anno.Timex {r | tiValue = f r.tiValue}
--   in
--     Lens.create get update
--
--
-- timexMod : Lens.Focus Anno.Timex (Maybe Anno.TimexMod)
-- timexMod =
--   let
--     get (Anno.Timex r) = r.tiMod
--     update f (Anno.Timex r) = Anno.Timex {r | tiMod = f r.tiMod}
--   in
--     Lens.create get update
--
--
-- timexAnchor : Lens.Focus Anno.Timex (Maybe Addr)
-- timexAnchor =
--   let
--     get (Anno.Timex r) = r.tiAnchor
--     update f (Anno.Timex r) = Anno.Timex {r | tiAnchor = f r.tiAnchor}
--   in
--     Lens.create get update
--
--
-- timexBeginPoint : Lens.Focus Anno.Timex (Maybe Addr)
-- timexBeginPoint =
--   let
--     get (Anno.Timex r) = r.tiBeginPoint
--     update f (Anno.Timex r) = Anno.Timex {r | tiBeginPoint = f r.tiBeginPoint}
--   in
--     Lens.create get update
--
--
-- timexEndPoint : Lens.Focus Anno.Timex (Maybe Addr)
-- timexEndPoint =
--   let
--     get (Anno.Timex r) = r.tiEndPoint
--     update f (Anno.Timex r) = Anno.Timex {r | tiEndPoint = f r.tiEndPoint}
--   in
--     Lens.create get update
--
--
-- timexQuant : Lens.Focus Anno.Timex (Maybe String)
-- timexQuant =
--   let
--     get (Anno.Timex r) = r.tiQuant
--     update f (Anno.Timex r) = Anno.Timex {r | tiQuant = f r.tiQuant}
--   in
--     Lens.create get update
--
--
-- timexFreq : Lens.Focus Anno.Timex (Maybe String)
-- timexFreq =
--   let
--     get (Anno.Timex r) = r.tiFreq
--     update f (Anno.Timex r) = Anno.Timex {r | tiFreq = f r.tiFreq}
--   in
--     Lens.create get update


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


-- -- | Change the treeMap of the model.
-- setTrees : TreeMap -> Model -> Model
-- setTrees treeDict model = Debug.crash "setTrees: not implemented"
-- --   let
-- --     treeId = case D.toList treeDict of
-- --       (id, tree) :: _ -> id
-- --       _ -> Debug.crash "setTrees: empty tree dictionary"
-- --   in
-- --     {model | trees = treeDict}
-- --       |> Lens.set (top => tree) treeId
-- --       |> Lens.set (bot => tree) treeId
-- -- --       |> updateSelect Top
-- -- --       |> updateSelect Bot


---------------------------------------------------
-- JSON Decoding
---------------------------------------------------


fileDecoder : Decode.Decoder File
fileDecoder =
  Decode.map6 File
    (Decode.field "treeMap" treeMapDecoder)
    (Decode.field "sentMap" sentMapDecoder)
    (Decode.field "partMap" partMapDecoder)
    (Decode.field "reprMap" reprMapDecoder)
    (Decode.field "turns" (Decode.list turnDecoder))
    (Decode.field "linkMap" linkMapDecoder)


turnDecoder : Decode.Decoder Turn
turnDecoder =
  let
    speakerDecoder = Decode.list Decode.string
    treesDecoder = Decode.map (mapKeys toInt) <| Decode.dict (Decode.nullable Decode.int)
  in
    Decode.map2 Turn
      (Decode.field "speaker" speakerDecoder)
      (Decode.field "trees" treesDecoder)


-- linkMapDecoder : Decode.Decoder (D.Dict Link LinkData)
-- linkMapDecoder = Decode.map D.fromList <| Decode.list linkDecoder
linkMapDecoder : Decode.Decoder (D.Dict Link Anno.Entity)
linkMapDecoder =
  let
    pairDecoder = Decode.map2 (\link ent -> (link, ent))
      (Decode.index 0 linkDecoder)
      -- (Decode.index 1 linkDataDecoder)
      (Decode.index 1 Anno.entityDecoder)
  in
    Decode.map D.fromList <| Decode.list pairDecoder


linkDecoder : Decode.Decoder Link
linkDecoder =
  Decode.map2 (\from to -> (from, to))
    (Decode.field "from" Anno.addrDecoder)
    (Decode.field "to" Anno.addrDecoder)


-- linkDataDecoder : Decode.Decoder LinkData
-- linkDataDecoder =
--   Decode.map (\x -> {signalAddr=x})
--     (Decode.field "signalAddr" (Decode.nullable Anno.addrDecoder))


-- treeMapDecoder : Decode.Decoder TreeMap
-- treeMapDecoder = Decode.map (mapKeys toInt) <| Decode.dict <|
--   Decode.map2 (\sent tree -> (sent, tree))
--     (Decode.index 0 Decode.string)
--     (Decode.index 1 treeDecoder)

treeMapDecoder : Decode.Decoder TreeMap
treeMapDecoder = Decode.map (mapKeys <| toInt) <| Decode.dict treeDecoder


sentMapDecoder : Decode.Decoder (D.Dict TreeIdBare Sent)
sentMapDecoder = Decode.map (mapKeys toInt) <| Decode.dict sentDecoder


sentDecoder : Decode.Decoder Sent
sentDecoder = Decode.list tokenDecoder


tokenDecoder : Decode.Decoder Token
tokenDecoder =
  Decode.map2 Token
    (Decode.field "orth" Decode.string)
    (Decode.field "afterSpace" Decode.bool)


partMapDecoder : Decode.Decoder (D.Dict TreeIdBare (S.Set TreeIdBare))
-- partMapDecoder = Decode.map (mapKeys toInt) <| Decode.dict <| Decode.set Decode.int
partMapDecoder =
    Decode.map (mapKeys toInt)
        <| Decode.dict
        <| Decode.map S.fromList
        <| Decode.list Decode.int


reprMapDecoder : Decode.Decoder (D.Dict TreeIdBare TreeIdBare)
reprMapDecoder =
    Decode.map (mapKeys toInt)
        <| Decode.dict Decode.int


treeDecoder : Decode.Decoder (R.Tree Node)
treeDecoder = R.treeDecoder nodeDecoder


nodeDecoder : Decode.Decoder Node
nodeDecoder = Decode.oneOf [internalDecoder, leafDecoder]


internalDecoder : Decode.Decoder Node
internalDecoder =
  Decode.map4 (\id val typ com -> Node {nodeId=id, nodeVal=val, nodeTyp=typ, nodeComment=com})
    (Decode.field "nodeId" Decode.int)
    (Decode.field "nodeVal" Decode.string)
    (Decode.field "nodeTyp" (Decode.nullable nodeTypDecoder))
    (Decode.field "nodeComment" Decode.string)


leafDecoder : Decode.Decoder Node
leafDecoder =
  Decode.map4 (\id val pos com -> Leaf {nodeId=id, nodeVal=val, leafPos=pos, nodeComment=com})
  -- Decode.map3 (\id pos com -> Leaf {nodeId=id, leafPos=pos, nodeComment=com})
    (Decode.field "leafId" Decode.int)
    (Decode.field "leafVal" Decode.string)
    (Decode.field "leafPos" Decode.int)
    (Decode.field "leafComment" Decode.string)


nodeTypDecoder : Decode.Decoder NodeAnnoTyp
nodeTypDecoder = Anno.entityDecoder
-- nodeTypDecoder = Decode.oneOf [nodeEventDecoder, nodeSignalDecoder, nodeTimexDecoder]


-- nodeEventDecoder : Decode.Decoder NodeAnnoTyp
-- nodeEventDecoder =
--   Decode.map (\ev -> NodeEvent ev)
--     (Decode.field "contents" Anno.eventDecoder)
--
--
-- nodeSignalDecoder : Decode.Decoder NodeAnnoTyp
-- nodeSignalDecoder =
--   Decode.map (\si -> NodeSignal si)
--     (Decode.field "contents" Anno.signalDecoder)
--
--
-- nodeTimexDecoder : Decode.Decoder NodeAnnoTyp
-- nodeTimexDecoder =
--   Decode.map (\ti -> NodeTimex ti)
--     (Decode.field "contents" Anno.timexDecoder)


---------------------------------------------------
-- JSON Encoding
---------------------------------------------------


encodeFile : File -> Encode.Value
encodeFile file =
  Encode.object
    [ ("tag", Encode.string "File")
    , ("treeMap", encodeTreeMap file.treeMap)
    , ("sentMap", encodeSentMap file.sentMap)
    , ("partMap", encodePartMap file.partMap)
    , ("reprMap", encodeReprMap file.reprMap)
    , ("turns", Encode.list (L.map encodeTurn file.turns))
    , ("linkMap", encodeLinkMap file.linkMap)
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


-- encodeLinkSet : D.Dict Link LinkData -> Encode.Value
-- encodeLinkSet = Encode.list << L.map encodeLink << S.toList
encodeLinkMap : D.Dict Link Anno.Entity -> Encode.Value
encodeLinkMap =
  let
    encodePair (link, ent) = Encode.list
      [ encodeLink link
      -- , encodeLinkData linkData ]
      , Anno.encodeEntity ent ]
      -- (encodeLink link, encodeLinkData linkData)
  in
    Encode.list << L.map encodePair << D.toList


encodeLink : Link -> Encode.Value
encodeLink (from, to) =
  Encode.object
    [ ("tag", Encode.string "Link")
    , ("from", Anno.encodeAddr from)
    , ("to", Anno.encodeAddr to)
    ]


-- encodeLinkData : LinkData -> Encode.Value
-- encodeLinkData x = Encode.object
--   [ ("tag", Encode.string "LinkData")
--   , ("signalAddr", Util.encodeMaybe Anno.encodeAddr x.signalAddr)
--   ]


-- encodeTreeMap : TreeMap -> Encode.Value
-- encodeTreeMap =
--   let
--     encodeSentTree (sent, tree) = Encode.list
--       [ encodeSent sent
--       , encodeTree tree ]
--     encodePair (treeId, sentTree) =
--       (toString treeId, encodeSentTree sentTree)
--   in
--     Encode.object << L.map encodePair << D.toList

encodeTreeMap : TreeMap -> Encode.Value
encodeTreeMap =
  let
    encodePair (treeId, tree) =
      (toString treeId, encodeTree tree)
  in
    Encode.object << L.map encodePair << D.toList


encodeSentMap : D.Dict TreeIdBare Sent -> Encode.Value
encodeSentMap =
  let
    encodePair (treeId, sent) =
      (toString treeId, encodeSent sent)
  in
    Encode.object << L.map encodePair << D.toList


encodeSent : Sent -> Encode.Value
encodeSent = Encode.list << L.map encodeToken


encodeToken : Token -> Encode.Value
encodeToken tok =
  Encode.object
    [ ("tag", Encode.string "Token")
    , ("orth", Encode.string tok.orth)
    , ("afterSpace", Encode.bool tok.afterSpace)
    ]


encodePartMap : D.Dict TreeIdBare (S.Set TreeIdBare) -> Encode.Value
encodePartMap =
  let
    encodeIdSet = Encode.list << L.map Encode.int << S.toList
    encodePair (treeId, idSet) =
      (toString treeId, encodeIdSet idSet)
  in
    Encode.object << L.map encodePair << D.toList


encodeReprMap : D.Dict TreeIdBare TreeIdBare -> Encode.Value
encodeReprMap =
  let
    encodePair (treeId, reprId) =
      (toString treeId, Encode.int reprId)
  in
    Encode.object << L.map encodePair << D.toList


encodeTree : R.Tree Node -> Encode.Value
encodeTree = R.encodeTree encodeNode


encodeNode : Node -> Encode.Value
encodeNode node = case node of
  Leaf r -> Encode.object
    [ ("tag", Encode.string "Leaf")
    , ("leafId", Encode.int r.nodeId)
    , ("leafVal", Encode.string r.nodeVal)
    , ("leafPos", Encode.int r.leafPos)
    , ("leafComment", Encode.string r.nodeComment)
    ]
  Node r -> Encode.object
    [ ("tag", Encode.string "Node")
    , ("nodeId", Encode.int r.nodeId)
    , ("nodeVal", Encode.string r.nodeVal)
    -- , ("nodeTyp", Util.encodeMaybe encodeNodeAnnoTyp r.nodeTyp)
    , ("nodeTyp", Util.encodeMaybe Anno.encodeEntity r.nodeTyp)
    , ("nodeComment", Encode.string r.nodeComment)
    ]


-- encodeNodeAnnoTyp : NodeAnnoTyp -> Encode.Value
-- encodeNodeAnnoTyp nodeTyp = case nodeTyp of
--   NodeEvent ev -> Encode.object
--     [ ("tag", Encode.string "NodeEvent")
--     , ("contents", Anno.encodeEvent ev) ]
--   NodeSignal si -> Encode.object
--     [ ("tag", Encode.string "NodeSignal")
--     , ("contents", Anno.encodeSignal si) ]
--   NodeTimex ti -> Encode.object
--     [ ("tag", Encode.string "NodeTimex")
--     , ("contents", Anno.encodeTimex ti) ]


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


-- | Retrieve the words (leaves) from a given tree and sort them by their
-- positions in the sentence.
getWords : R.Tree Node -> List LeafNode
getWords tree =
    L.map Tuple.first <| getWordsPos tree
--     let
--         leaf node = case node of
--                         Leaf x -> Just x
--                         Node _ -> Nothing
--     in
--         List.sortBy (\x -> x.leafPos)
--             <| List.filterMap leaf
--             <| R.flatten tree


-- | Retrieve the words (leaves) from a given tree and sort them by their
-- positions in the sentence.  The result contains information about the
-- POS tags.
getWordsPos : R.Tree Node -> List (LeafNode, Pos)
getWordsPos =
  let
    go pos xs = case xs of
      [] -> []
      node :: nodes -> case node of
        Leaf r -> (r, pos) :: go "" nodes
        Node r -> go r.nodeVal nodes
    getList = go "" << L.reverse << R.flatten
  in
    getList


-- | Get the subtree indicated by the given address.
subTreeAt : Addr -> Focus -> Model -> R.Tree Node
subTreeAt (treeId, theNodeId) win model =
    let
        pred x = Lens.get nodeId x == theNodeId
        tree = getTree win treeId model
    in
        case R.getSubTree pred tree of
            Nothing -> Debug.crash "View.subTreeAt: no node with the given ID"
            Just t  -> t


-- -- | Re-position the leaves of the tree.
-- rePOS : R.Tree Node -> R.Tree Node
-- rePOS =
--     let
--         update pos x =
--             case x of
--                 Node r -> (pos, Node r)
--                 Leaf r -> (pos+1, Leaf {r | leafPos=pos})
--     in
--         Tuple.second << R.mapAccum update 0


-- | Add the given shift to all positions in the leaves.
addPOS : Int -> R.Tree Node -> R.Tree Node
addPOS shift = updatePOS <| \k -> k + shift


-- | Apply the given function to all positions in the leaves.
updatePOS : (Int -> Int) -> R.Tree Node -> R.Tree Node
updatePOS f =
    let update = Lens.update leafPos f
    in  R.map update


findMaxID : R.Tree (Maybe Node) -> Maybe NodeId
findMaxID tree =
    List.maximum
      <| L.map (\x -> Lens.get nodeId x)
      <| Util.catMaybes
      <| R.flatten tree
