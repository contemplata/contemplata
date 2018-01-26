module Edit.View.Tree exposing
  (
  -- * Viewing
    viewTree
  , viewLine

  -- * Positioning
  , nodePos
  , positionTree

  -- * Configuration
  , stdWidth
  , defLineCfg
  )


import Char
import List as L
import Set as S
import Dict as D
import Focus as Lens
import Tuple exposing (first, second)
import Mouse exposing (Position)
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Svg as Svg
import Svg.Attributes as Svg

import Rose as R
import Util as Util
import Config as Cfg

import Edit.Core as C
import Edit.Model as M
import Edit.Message.Core exposing (Msg(..))
import Edit.View.Circle as Circle
import Edit.View.Box as Box


---------------------------------------------------
-- Drawing trees
---------------------------------------------------


-- | View tree in the specified workspace.
viewTree : C.Focus -> M.Model -> Html.Html Msg
viewTree focus model =

  let

    win = M.selectWin focus model
    treeId = M.getReprId focus win.tree model
    tree = M.getTree focus treeId model
    file = Lens.get (M.fileLens focus) model

    inTree treeId0 ((treeId1, nodeId), link) =
      if treeId0 == treeId1
      then Just (nodeId, S.singleton link)
      else Nothing
    getLinkNodes select =
      Util.fromListWith S.union <|
      L.filterMap (inTree treeId) <|
      L.map (\link -> (select link, link)) <|
      D.keys file.linkMap

    config =
      { focus = focus
      , selMain = win.selMain
      , selAux = win.selAux
      , linkIn = getLinkNodes second
      , linkOut = getLinkNodes first
      , selLink = model.selLink
      }
    positionedTree =
      positionTree (M.getPosition focus model)
        <| R.withWidth stdWidth Cfg.stdMargin tree
    treeCanvas =
      drawTree config
        <| markMisplaced first
        <| positionedTree
    rootMarkerCanvas =
        drawRootMarker focus model
            <| Tuple.second
            <| R.label positionedTree

  in

    Html.div []
        [ treeCanvas
        , rootMarkerCanvas
        ]


---------------------------------------------------
-- Drawing root marker
---------------------------------------------------


drawRootMarker
   : C.Focus
  -> M.Model
  -> Position -- ^ Position or the root
  -> Html.Html Msg
drawRootMarker focus model rootPos =
  let
    dim = model.dim

    width = ((dim.width * dim.widthProp) // 100)
            -- - Cfg.dmzSize
    topHeight = (dim.height * dim.heightProp) // 100
    botHeight = dim.height - topHeight
    height = -- (\h -> h - Cfg.dmzSize) <|
      case focus of
        C.Top -> topHeight
        C.Bot -> botHeight

    center =
        { x = width // 2
        , y = height // 2
        }
    lineToRoot = {beg = center, end = rootPos}
    box =
        { leftTop =
              {x = 0, y = 0}
        , rightBot =
              { x = width
              , y = height
              }
        }
    defCircleCfg = Circle.defCircleCfg
    circleCfg =
      { defCircleCfg
      | width = Cfg.rootMarkerSize
      , height = Cfg.rootMarkerSize }
  in
    case Box.boxLineIntersection box lineToRoot of
      Nothing ->
        Html.div [] []
      Just markerPos ->
        Html.div [] [Circle.drawCircle circleCfg markerPos]


---------------------------------------------------
-- Drawing trees
---------------------------------------------------


type alias TreeCfg =
  { focus : C.Focus
  -- ^ Which window is it in?
  , selMain : Maybe C.NodeId
  -- ^ Selected main
  , selAux : S.Set C.NodeId
  -- ^ Selected auxiliary
  , linkIn : D.Dict C.NodeId (S.Set C.Link)
  -- ^ The set of node IDs with an in-going relation
  , linkOut : D.Dict C.NodeId (S.Set C.Link)
  -- ^ The set of node IDs with an out-going relation
  , selLink : Maybe C.Link
  -- ^ The currently selected link (if any)
  }


drawTree
   : TreeCfg
  -> R.Tree ((M.Node, Position), NodeTyp) -- ^ Tree to draw
  -> Html.Html Msg
drawTree cfg (R.Node ((node, pos), mark) subTrees) =
  let
    lineCfg = { defLineCfg
      | strokeWidth = 2
      , opacity = "0.7" }
    drawForest forest = case forest of
      [] -> []
      t :: ts ->
        drawTree cfg t
          :: viewLine lineCfg pos (second <| first <| R.label t)
          :: drawForest ts
  in
    Html.div []
      (  drawNode cfg node pos mark
      :: drawForest subTrees )


-- | Draw a tree node.
drawNode
   : TreeCfg
  -> M.Node
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawNode cfg node =
    case node of
        M.Node r -> drawInternal cfg r
        M.Leaf r -> drawLeaf cfg r


-- | Draw an internal tree node.
drawInternal
   : TreeCfg
  -> M.InternalNode
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawInternal cfg node at mark =
  let
    width = intWidth node
    height = Cfg.nodeHeight
    nodeId = node.nodeId

    hasType x =
        case x.nodeTyp of
            Nothing -> False
            _ -> True
    nodeColor =
        if S.member nodeId cfg.selAux || Just nodeId == cfg.selMain
        then Cfg.nodeSelectColor
        else if hasType node
        then Cfg.nodeColor (Maybe.map .name node.nodeTyp)
        else if mark == Misplaced
        then Cfg.nodeMisplacedColor
        else Cfg.nodeRegularColor

    auxStyle =
      [ "background-color" :> nodeColor ]
      ++
      ( if Just nodeId == cfg.selMain
          then ["border" :> "solid", "border-color" :> "black"]
          else ["border" :> "none"] )
    htmlLeaf =
        [ Html.text node.nodeVal
        , case node.nodeTyp of
            Nothing -> Html.sub [] []
            Just en -> Html.sub []
                       [ Html.text <|
                         String.map Char.toUpper <|
                         String.left 2 en.name ]
        ]

    nodeDiv =
      Html.div
        [ nodeMouseDown cfg.focus (M.Node node)
        , Atts.class "noselect"
        , Atts.style <| auxStyle ++
            [ "cursor" :> "pointer"
            -- , "opacity" :> "1.0"

            , "width" :> px width
            , "height" :> px height
            , "border-radius" :> "40%" -- "4px"
            , "position" :> "absolute"

            , "left" :> px (at.x - width // 2)
            , "top" :> px (at.y - height // 2)

            , "color" :> "white"
            , "display" :> "flex"
            , "align-items" :> "center"
            , "justify-content" :> "center"
            ]
        ]
        [Html.p [] htmlLeaf]

    defCircleCfg = Circle.defCircleCfg
    circCfg isMain =
      { defCircleCfg
      | opacity = Cfg.relMarkerOpacity
      , width = Cfg.relMarkerSize isMain
      , height = Cfg.relMarkerSize isMain }

    circPos foc =
      case foc of
        C.Top ->
          { x = at.x - round (toFloat width / Cfg.relMarkerDist)
          , y = at.y - round (toFloat height / Cfg.relMarkerDist) }
        C.Bot ->
          { x = at.x - round (toFloat width / Cfg.relMarkerDist)
          , y = at.y + round (toFloat height / Cfg.relMarkerDist) }
    circ foc linkSet =
      let newLink = getNextLink cfg.selLink linkSet in
      let isMain = cfg.focus == other foc in
      let atts = if isMain then ["cursor" :> "pointer"] else [] in
      let circleStyle = Circle.circleStyleExt atts (circCfg isMain) (circPos foc) in
      if cfg.focus == other foc
      then Html.div [circleStyle, Events.onClick (FocusLink newLink)] []
      else Html.div [circleStyle] []

    relDivUp =
      case D.get node.nodeId cfg.linkIn of
          Just linkSet -> [circ C.Top linkSet]
          Nothing -> []
    relDivDown =
      case D.get node.nodeId cfg.linkOut of
          Just linkSet -> [circ C.Bot linkSet]
          Nothing -> []

  in

    Html.div [] (nodeDiv :: relDivUp ++ relDivDown)


-- | Draw a leaf tree node.
drawLeaf
   : TreeCfg
  -> M.LeafNode
  -> Position
  -> NodeTyp -- ^ Should be marked as misplaced?
  -> Html.Html Msg
drawLeaf cfg node at mark =
  let
    width = leafWidth node
    height = Cfg.nodeHeight
    nodeId = node.nodeId
    auxStyle =
      ( if S.member nodeId cfg.selAux || Just nodeId == cfg.selMain
        then ["background-color" :> "#BC0000"]
        else if mark == Misplaced
        then ["background-color" :> "#EF597B"]
        else ["background-color" :> "#1F5C9A"] ) -- "#1F9A6D"
      ++
      ( if Just nodeId == cfg.selMain
          then ["border" :> "solid", "border-color" :> "black"]
          else ["border" :> "none"] )
    htmlLeaf =
        [ Html.text node.nodeVal
        , Html.sub [] [Html.text <| toString node.leafPos] ]
  in
    Html.div
      [ nodeMouseDown cfg.focus (M.Leaf node)
      , Atts.class "noselect"
      , Atts.style <| auxStyle ++
          [ "cursor" :> "pointer"
          -- , "opacity" :> "1.0"

          , "width" :> px width
          , "height" :> px height
          , "border-radius" :> "40%" -- "4px"
          , "position" :> "absolute"
          -- , "left" :> px (at.x - nodeWidth // 2)
          -- , "top" :> px (at.y - nodeHeight // 2)
          , "left" :> px (at.x - width // 2)
          , "top" :> px (at.y - height // 2)

          , "color" :> "white"
          , "display" :> "flex"
          , "align-items" :> "center"
          , "justify-content" :> "center"
          ]
      ]
      [Html.p [] htmlLeaf]


-- | Get the next link to focus on.
getNextLink : Maybe C.Link -> S.Set C.Link -> C.Link
getNextLink maySelLink linkSet =
    let
        err = "View.Tree.getNextLink: empty linkset"
        takeFirst set =
            case S.toList set of
                x :: _ -> Just x
                [] -> Nothing
        result =
            case maySelLink of
                Nothing -> takeFirst linkSet
                Just current ->
                    Util.mappend
                        (takeFirst <| S.filter (\link -> link > current) linkSet)
                        (takeFirst linkSet)
    in
        case result of
            Just x -> x
            Nothing -> Debug.crash err


other : C.Focus -> C.Focus
other foc =
    case foc of
        C.Top -> C.Bot
        C.Bot -> C.Top

---------------------------------------------------
-- Lines
---------------------------------------------------


type alias LineCfg =
  { color : String
  , strokeWidth : Int
  , zindex : Int
  , strokeDasharray : Maybe String
  , opacity : String
  , isArrow : Bool
  }


defLineCfg : LineCfg
defLineCfg =
  { color = "black"
  , strokeWidth = 1
  , zindex = -1
  , strokeDasharray = Nothing
  , opacity = "1"
  , isArrow = False
  }


viewLine : LineCfg -> Position -> Position -> Html.Html Msg
viewLine cfg beg end =
  let
    -- Note that the width is handled in a tricky way. This is to handle the
    -- case where the line is vertical. The case where the line is horizontal is
    -- not handled.
    width  = (\x->x+1) <| abs <| end.x - beg.x
    height = abs <| end.y - beg.y
    (x1, x2) = case end.x >= beg.x of
             True  -> ("1", toString width)
             False -> (toString width, "1")
    (y1, y2) = case end.y >= beg.y of
             True  -> ("0", toString height)
             False -> (toString height, "0")
    dash = case cfg.strokeDasharray of
      Nothing -> []
      Just x  -> [Svg.strokeDasharray x]
    line = Svg.line
      ( [ Svg.stroke cfg.color
        , Svg.strokeWidth (toString cfg.strokeWidth)
        , Svg.opacity cfg.opacity
        , Svg.x1 x1, Svg.y1 y1, Svg.x2 x2, Svg.y2 y2 ]
        ++ dash
      )
      []
    svg = Svg.svg
      [ Svg.width (toString <| (\x->x+1) <| width)
      , Svg.height (toString height)
      ] [line]
  in
    Html.div
      [ Atts.style
          [ "position" :> "absolute"
          , "left" :> px (min beg.x end.x)
          , "top" :> px (min beg.y end.y)
          , "pointer-events" :> "none"
          , "z-index" :> toString cfg.zindex
          ]
      ]
      [svg]


---------------------------------------------------
-- Configuration
---------------------------------------------------


-- | Width of an internal node.
intWidth : M.InternalNode -> Int
intWidth r =
  let
    sub =
      case r.nodeTyp of
        Nothing -> ""
        Just en -> String.left 2 en.name
    (txt, ix) = (r.nodeVal, sub)
  in
    max 30 <| String.length txt * 11 + String.length ix * 7


-- | Width of a leaf node.
leafWidth : M.LeafNode -> Int
leafWidth r =
  let
    (txt, ix) = (r.nodeVal, toString r.leafPos)
  in
    max 30 <| String.length txt * 10 + String.length ix * 7


-- | Width of a node.
stdWidth : M.Node -> Int
stdWidth x =
  case x of
    M.Node r -> intWidth r
    M.Leaf r -> leafWidth r


---------------------------------------------------
-- Events
---------------------------------------------------


nodeMouseDown : C.Focus -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win <| Lens.get M.nodeId x)


---------------------------------------------------
-- Determine which nodes should be marked
-- as "misplaced" or "non-projective"
---------------------------------------------------


-- | To mark nodes as misplaced.
type NodeTyp = Normal | Misplaced


-- | Determine which leaves should be marked as "misplaced".
markMisplaced
    : (a -> M.Node)
    -> R.Tree a
    -> R.Tree (a, NodeTyp)
markMisplaced getNode =
    let
        markTree prevSpan (R.Node (wrapper, span) forest) =
            let
                nodeTyp = case prevSpan of
                    Nothing -> Normal
                    Just (_, prevEnd) ->
                        if prevEnd < first span
                        then Normal
                        else Misplaced
            in
                (Just span, R.Node (wrapper, nodeTyp) (markForest forest))
        markForest =
            second << Util.mapAccumL markTree Nothing
        markRoot (R.Node (wrapper, span) forest) =
            R.Node (wrapper, Normal) (markForest forest)
    in
        propagateMarks << markRoot << addSpans getNode


-- | Propagate the markings downward to the leaves.
propagateMarks
    : R.Tree (a, NodeTyp)
    -> R.Tree (a, NodeTyp)
propagateMarks =
    let
        propTyp typ =
            case typ of
                Normal -> Nothing
                x -> Just x
        markTree fromUp (R.Node (wrapper, typ) forest) =
            case fromUp of
                Nothing ->
                    let newTyp = propTyp typ
                    in  R.Node (wrapper, typ) (markForest newTyp forest)
                Just newTyp ->
                    R.Node (wrapper, newTyp) (markForest fromUp forest)
        markForest : Maybe NodeTyp -> R.Forest (a, NodeTyp) -> R.Forest (a, NodeTyp)
        markForest fromUp = L.map (markTree fromUp)
    in
        markTree Nothing


-- | Determine node spans.
addSpans
    : (a -> M.Node)
    -> R.Tree a
    -> R.Tree (a, (Int, Int))
addSpans getNode =
    let
        go (R.Node wrapper forest0) =
            case getNode wrapper of
                M.Node _ ->
                    let forest = L.map go forest0
                        beg = spanBeg forest
                        end = spanEnd forest
                        span = (beg, end)
                    in  R.Node (wrapper, span) forest
                M.Leaf r ->
                    let span = (r.leafPos, r.leafPos)
                    in  R.Node (wrapper, span) []
        spanBeg xs =
            case L.minimum <| L.map (first << second << R.label) xs of
                Nothing -> Debug.crash "Edit.View.addSpans: spanBeg"
                Just x  -> x
        spanEnd xs =
            case L.maximum <| L.map (second << second << R.label) xs of
                Nothing -> Debug.crash "Edit.View.addSpans: spanEnd"
                Just x  -> x
    in
        go


---------------------------------------------------
-- Positioning
---------------------------------------------------


-- | Position a given tree. This function calculates the positions of the
-- individual nodes in the given tree, based on their widths (see also
-- `R.withWidth`).
positionTree : Position -> R.Tree (M.Node, R.Width) -> R.Tree (M.Node, Position)
positionTree pos (R.Node (node, rootWidth) subTrees) =
  let
    forestWidth = List.sum <| L.map (R.label >> second) subTrees
    positionF w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = second <| R.label t
          tpos = {x = w0 + tw // 2, y = pos.y + Cfg.moveDown}
        in
          positionTree tpos t :: positionF (w0 + tw) ts
  in
    R.Node (node, pos) (positionF (pos.x - forestWidth // 2) subTrees)


-- | Retrieve the position of a node in a given tree.
nodePos : C.NodeId -> R.Tree (M.Node, Position) -> Maybe Position
nodePos nodeId tree = Maybe.map second <|
  Util.find
    (\node -> Lens.get M.nodeId (first node) == nodeId)
    (R.flatten tree)


---------------------------------------------------
-- Utils
---------------------------------------------------


(:>) : a -> b -> (a, b)
(:>) = (,)


px : Int -> String
px number =
  toString number ++ "px"
