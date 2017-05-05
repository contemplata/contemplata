module View exposing (view)


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Svg as Svg
import Svg.Attributes as Svg
-- import Svg as SVG -- to avoid ambiguity in a couple of places
import Set as S
import Dict as D
import List as L
import Maybe as Maybe
import Tuple exposing (first, second)
import Mouse exposing (Position)

import Rose as R
import Util as Util
import Model as M
import Message as Msg
import Message exposing (Msg(..))
import Config as Cfg


view : M.Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "width" => "100%"
        , "height" => "100%"
        ]
    ]
    ( [ stylesheet
      , viewWindow M.Top model, viewSideWindow M.Top model
      , viewWindow M.Bot model, viewSideWindow M.Bot model
      ] ++ viewLinks model )


stylesheet =
  let
    tag = "link"
    attrs =
      [ Atts.attribute "rel" "stylesheet"
      , Atts.attribute "property" "stylesheet"
      -- , attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      , Atts.attribute "href" "style.css"
      ]
    children = []
  in
    Html.node tag attrs children


-- | The view of the top window.
viewWindow : M.Focus -> M.Model -> Html.Html Msg
viewWindow win model =
  Html.div
  [ backMouseDown win
  , winOnFocus win
  -- , topOnResize
--   , case win of
--       M.Top -> Atts.autofocus True
--       M.Bot -> Atts.autofocus False
  , case win of
      M.Top -> Atts.id "top"
      M.Bot -> Atts.id "bot"

  -- @tabindex required to make the div register keyboard events
  , Atts.attribute "tabindex" "1"
  , backKeyDown
  , backKeyUp

  , Atts.style
    [ "position" => "absolute"
    , "width" => (toString (100 - Cfg.sideSpace) ++ "%") -- "100%"
    -- , "height" => "50%"
    , "height" => case win of
        M.Top -> toString model.dim.heightProp ++ "%"
        M.Bot -> toString (100 - model.dim.heightProp) ++ "%"
    , case win of
        M.Top -> "top" => "0"
        M.Bot -> "bottom" => "0"
    -- Overflow is a very important attribute (it makes the scrollbars to appear
    -- if set to "auto") which makes sure that the trees do not go beyond the
    -- specified subwindows.  We set to "hidden" so that tracing links is easy
    -- (otherwise, special care has to be taken w.r.t. scrollbars).
    , "overflow" => "hidden"
    , "background-color" => backColor win model
    , "opacity" => "1.0"
    -- z-index important because of its interactions with how the edges are
    -- drawn.
    , "z-index" => "-1"
    -- , "border" => "1px black solid"
    ]
  ]
  -- [background, tr]
  [ viewTree win model
  , viewTreeId win model ]


-- | Determine the background color.
backColor : M.Focus -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"



-- | View tree in the specified window.
viewTree : M.Focus -> M.Model -> Html.Html Msg
viewTree focus model =
  let
    win = M.selectWin focus model
    tree = M.getTree win.tree model
    -- select = M.selAll win
  in
    drawTree
      focus win.selMain win.selAux
      <| positionTree (M.getPosition win)
      <| R.withWidth Cfg.stdWidth Cfg.stdMargin tree


viewTreeId : M.Focus -> M.Model -> Html.Html Msg
viewTreeId win model =
  let
    txt0 = toString (M.treePos win model)
      ++ "/"
      ++ toString (M.treeNum model)
    txt = txt0 ++ case win of
      M.Top -> " (" ++ toString model.dim.height ++ ")"
      M.Bot -> " (" ++ toString model.dim.width ++ ")"
  in
    Html.div
      [ Atts.class "noselect"
      , Atts.style
        [ "position" => "absolute"
        -- , "width" => "5%"
        -- , "height" => "5%"
        , "top" => px 10
        , "left" => px 10
        ]
      ]
      [ Html.text txt ]


---------------------------------------------------
-- Drawing trees
---------------------------------------------------


drawTree
   : M.Focus -- ^ Which window is it in?
  -> Maybe M.NodeId -- ^ Selected main
  -> S.Set M.NodeId -- ^ Selected auxiliaries
  -> R.Tree (M.Node, Position) -- ^ Tree to draw
  -> Html.Html Msg
drawTree focus selMain selAux (R.Node (node, pos) subTrees) =
  let
    lineCfg = { defLineCfg
      | strokeWidth = 2
      , opacity = "0.7" }
      -- , color = "gray" }
    drawForest forest = case forest of
      [] -> []
      t :: ts ->
        drawTree focus selMain selAux t
          :: viewLine lineCfg pos (R.getRootSnd t)
          :: drawForest ts
  in
    Html.div []
      (  drawNode selMain selAux focus pos node
      :: drawForest subTrees )


drawNode
   : Maybe M.NodeId
  -> S.Set M.NodeId
  -> M.Focus
  -> Position
  -> M.Node
  -> Html.Html Msg
drawNode selMain selAux focus at node =
  let
    -- width = nodeWidth
    width = Cfg.stdWidth node
    height = Cfg.nodeHeight
    auxStyle =
      ( if S.member node.nodeId selAux || Just node.nodeId == selMain
          then ["background-color" => "#BC0000"]
          else ["background-color" => "#3C8D2F"] )
      ++
      ( if Just node.nodeId == selMain
          then ["border" => "solid", "border-color" => "black"]
          else ["border" => "none"] )
  in
    Html.div
      [ nodeMouseDown focus node
      , Atts.class "noselect"
      , Atts.style <| auxStyle ++
          [ "cursor" => "pointer"
          -- , "opacity" => "1.0"

          , "width" => px width
          , "height" => px height
          , "border-radius" => "40%" -- "4px"
          , "position" => "absolute"
          -- , "left" => px (at.x - nodeWidth // 2)
          -- , "top" => px (at.y - nodeHeight // 2)
          , "left" => px (at.x - width // 2)
          , "top" => px (at.y - height // 2)

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ Html.text node.nodeVal ]
--       [ Html.div
--           [ Atts.attribute "contenteditable" "true" ]
--           [ Html.text node.nodeVal ]
--       ]


---------------------------------------------------
-- Side windows
---------------------------------------------------


 -- | The view of the top-side window.
viewSideWindow : M.Focus -> M.Model -> Html.Html Msg
viewSideWindow win model =
  let
    selected = case win of
      M.Top -> model.top.selMain
      M.Bot -> model.bot.selMain
    (condAtts, event) = case selected of
      Just nodeId ->
        ( [Atts.value (M.getLabel nodeId win model)]
        , ChangeLabel nodeId win )
      Nothing ->
        ( [Atts.disabled True, Atts.placeholder "<label>"]
        , \_ -> Msg.dummy )
  in
    Html.div
      [ Atts.style
        [ "position" => "absolute"
        , "width" => (toString Cfg.sideSpace ++ "%")
        -- , "height" => "50%"
        , "height" => case win of
            M.Top -> toString model.dim.heightProp ++ "%"
            M.Bot -> toString (100 - model.dim.heightProp) ++ "%"
        , "right" => "0"
        , case win of
            M.Top -> "top" => "0"
            M.Bot -> "bottom" => "0"
        , "overflow" => "auto"
        ]
      ]
      [ Html.input
          ( [ Events.onInput event
            , Atts.id <| case win of
                M.Top -> Cfg.editLabelName True
                M.Bot -> Cfg.editLabelName False
            , Atts.style
              [ -- "position" => "absolute"
                "width" => "95%"
              ]
            ] ++ condAtts
          )
          []
      ]


---------------------------------------------------
-- Links
---------------------------------------------------


viewLinks : M.Model -> List (Html.Html Msg)
viewLinks model =
  L.concatMap
    (viewLink model)
    (S.toList model.links)


viewLink
   : M.Model
  -> (M.Addr, M.Addr)
  -> List (Html.Html Msg)
viewLink model (from, to) =
  let
    top = model.top
    bot = model.bot
    dim = model.dim

    mainWidth = (dim.width * (100 - Cfg.sideSpace)) // 100
    topHeight = (dim.height * dim.heightProp) // 100
    botHeight = dim.height - topHeight

    trimTop pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= topHeight - Cfg.dmzSize
      then Just pos
      else Nothing

    trimBot pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= botHeight - Cfg.dmzSize
      then Just {pos | y = pos.y + topHeight}
      else Nothing

  in

    if
      top.tree == first from &&
      bot.tree == first to
    then
      viewLinkDir model (top, bot) (trimTop, trimBot) (from, to)
    else if
      bot.tree == first from &&
      top.tree == first to &&
      top.tree /= bot.tree
    then
      viewLinkDir model (bot, top) (trimBot, trimTop) (from, to)
    else
      []


viewLinkDir
   : M.Model
  -> (M.Window, M.Window)
  -> (Position -> Maybe Position, Position -> Maybe Position)
  -> (M.Addr, M.Addr)
  -> List (Html.Html Msg)
viewLinkDir model (top, bot) (shiftTop, shiftBot) (from, to) =
  let
    nodePos1 nodeId pos tree = nodePos nodeId
      <| positionTree pos
      <| R.withWidth Cfg.stdWidth Cfg.stdMargin tree
    begPos = Maybe.andThen shiftTop <| nodePos1
      (second from)
      (M.getPosition top)
      (M.getTree top.tree model)
    endPos = Maybe.andThen shiftBot <| nodePos1
      (second to)
      (M.getPosition bot)
      (M.getTree bot.tree model)
    lineCfg = { defLineCfg
      | strokeDasharray = Just Cfg.linkDasharray
      , strokeWidth = Cfg.linkWidth
      , opacity = Cfg.linkOpacity }
    circleCfg = { defCircleCfg
      | opacity = Cfg.linkOpacity
      , width = Cfg.linkHeadSize
      , height = Cfg.linkHeadSize }
  in
    case (begPos, endPos) of
      (Just p, Just q) ->
        let
          v1 = trimBeg Cfg.linkTailDist
            <| trimEnd (first Cfg.linkHeadDist)
            <| {beg=p, end=q}
          v2 = trimEnd (second Cfg.linkHeadDist) {beg=p, end=q}
        in
          [ viewLine lineCfg v1.beg v1.end
          , drawCircle circleCfg v2.end ]
      _ -> []


---------------------------------------------------
-- Circles
---------------------------------------------------


type alias CircleCfg =
  { color : String
  , opacity : String
  , height : Int
  , width : Int
  }


defCircleCfg : CircleCfg
defCircleCfg =
  { color = "black"
  , opacity = "1"
  , height = 10
  , width = 10
  }


drawCircle : CircleCfg -> Position -> Html.Html msg
drawCircle cfg at =
  Html.div
    [ Atts.style
        [ "background-color" => cfg.color
        , "opacity" => cfg.opacity
        , "width" => px cfg.width
        , "height" => px cfg.height
        , "border-radius" => "50%"
        , "position" => "absolute"
        , "left" => px (at.x - cfg.width // 2)
        , "top" => px (at.y - cfg.height // 2)
        ]
    ]
    []


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
          [ "position" => "absolute"
          , "left" => px (min beg.x end.x)
          , "top" => px (min beg.y end.y)
          , "pointer-events" => "none"
          , "z-index" => toString cfg.zindex
          ]
      ]
      [svg]


---------------------------------------------------
-- Events
---------------------------------------------------


nodeMouseDown : M.Focus -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win x.nodeId)


backMouseDown : M.Focus -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)


-- backDoubleClick : M.Focus -> Html.Attribute Msg
-- backDoubleClick win =
--   Events.onDoubleClick (Focus win)


backKeyDown : Html.Attribute Msg
backKeyDown =
  let
    tag code = case code of
      -- "PgUp" and "PgDown"
      33 -> Previous
      34 -> Next

      -- "Del"
      46 -> Delete

      -- "a"
      65 -> Add

      -- "+" and "-"
      107 -> Increase True
      109 -> Increase False

      -- "e"
      69 -> EditLabel

      -- "ctrl"
      17 -> CtrlDown

      _  -> Msg.dummy
  in
    onKeyDown tag


backKeyUp : Html.Attribute Msg
backKeyUp =
  let
    tag code = case code of

      -- "ctrl"
      17 -> CtrlUp

      _  -> Msg.dummy
  in
    onKeyUp tag


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
  Events.on "keyup" (Decode.map tagger Events.keyCode)


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  Events.on "keydown" (Decode.map tagger Events.keyCode)


winOnFocus : M.Focus -> Html.Attribute Msg
winOnFocus win =
  Events.onFocus (Focus win)


-- | Decode the height of a given element.
offsetHeight : Decode.Decoder Int
offsetHeight =
  Decode.at ["target", "offsetHeight"] Decode.int


---------------------------------------------------
-- Positioning
---------------------------------------------------


-- | Position a given tree. This function calculates the positions of the
-- individual nodes in the given tree.
positionTree : Position -> R.Tree (M.Node, R.Width) -> R.Tree (M.Node, Position)
positionTree pos (R.Node (node, rootWidth) subTrees) =
  let
    forestWidth = List.sum <| L.map R.getRootSnd subTrees
    positionF w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = R.getRootSnd t
          tpos = {x = w0 + tw // 2, y = pos.y + Cfg.moveDown}
        in
          positionTree tpos t :: positionF (w0 + tw) ts
  in
    R.Node (node, pos) (positionF (pos.x - forestWidth // 2) subTrees)


-- | Retrieve the position of a node in a given tree.
nodePos : M.NodeId -> R.Tree (M.Node, Position) -> Maybe Position
nodePos nodeId tree = Maybe.map second <|
  Util.find
    (\node -> (first node).nodeId == nodeId)
    (R.flatten tree)


---------------------------------------------------
-- Vectors
---------------------------------------------------


type alias Vect = {beg : Position, end : Position}


-- | Length of a vector.
length : Vect -> Float
length {beg, end} =
  let
    x = end.x - beg.x
    y = end.y - beg.y
  in
    sqrt <| toFloat (x*x + y*y)


-- | Inverse a vector.
inverse : Vect -> Vect
inverse {beg, end} = {beg=end, end=beg}


-- | Shorten the end of a given vector by a given length.
trimEnd : Int -> Vect -> Vect
trimEnd trim ({beg, end} as v) =
  let
    trimProp = toFloat trim / length v
    restProp = 1.0 - trimProp
    x = beg.x + round (toFloat (end.x - beg.x) * restProp)
    y = beg.y + round (toFloat (end.y - beg.y) * restProp)
  in
    {beg=beg, end={x=x, y=y}}


-- | Shorten the beinning of a given vector by a given length.
trimBeg : Int -> Vect -> Vect
trimBeg trim v = inverse <| trimEnd trim <| inverse v


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)


px : Int -> String
px number =
  toString number ++ "px"
