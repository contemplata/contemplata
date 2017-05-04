module View exposing (view)


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Svg as Svg
import Svg.Attributes as Svg
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


(=>) : a -> b -> (a, b)
(=>) = (,)


-- | View tree in the specified window.
viewTree : M.Focus -> M.Model -> Html.Html Msg
viewTree focus model =
  let
    win = M.selectWin focus model
    tree = M.getTree win.tree model
  in
    drawTree
      focus win.select
      <| positionTree (M.getPosition win)
      <| R.withWidth Cfg.stdWidth Cfg.stdMargin tree


-- | Determine the background color.
backColor : M.Focus -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"


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


 -- | The view of the top-side window.
viewSideWindow : M.Focus -> M.Model -> Html.Html Msg
viewSideWindow win model =
  let
    selected = case win of
      M.Top -> model.top.select
      M.Bot -> model.bot.select
    (condAtts, event) = case S.toList selected of
      [nodeId] ->
        ( [Atts.value (M.getLabel nodeId win model)]
        , ChangeLabel nodeId win )
      _ ->
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
  in
    case (begPos, endPos) of
      (Just p, Just q) ->
        [ drawLine p q
        ]
      _ -> []


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


-- viewMetaLine Position -> Position -> Html.Html Msg
-- viewMetaLine beg end =


drawLine : Position -> Position -> Html.Html Msg
drawLine beg end =
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
    line = Svg.line
      [ Svg.stroke "black", Svg.strokeWidth "2"
      , Svg.x1 x1, Svg.y1 y1, Svg.x2 x2, Svg.y2 y2 ] []
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
          , "z-index" => "-1"
          ]
      ]
      [svg]


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


drawTree
   : M.Focus -- ^ Which window is it in?
  -> S.Set M.NodeId -- ^ Selected nodes
  -> R.Tree (M.Node, Position) -- ^ Tree to draw
  -> Html.Html Msg
drawTree focus select (R.Node (node, pos) subTrees) =
  let
    drawForest forest = case forest of
      [] -> []
      t :: ts ->
        drawTree focus select t
          :: drawLine pos (R.getRootSnd t)
          :: drawForest ts
  in
    Html.div []
      (  drawNode select focus pos node
      :: drawForest subTrees )


drawNode : S.Set M.NodeId -> M.Focus -> Position -> M.Node -> Html.Html Msg
drawNode select focus at node =
  let
    -- width = nodeWidth
    width = Cfg.stdWidth node
    height = Cfg.nodeHeight
  in
    Html.div
      [ nodeMouseDown focus node
      , Atts.class "noselect"
      , Atts.style
          [
            if S.member node.nodeId select
              then "background-color" => "#BC0000"
              else "background-color" => "#3C8D2F"
          , "cursor" => "pointer"
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


nodeMouseDown : M.Focus -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win x.nodeId)


---------------------------------------------------
-- Background events
---------------------------------------------------


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

      _  -> Msg.dummy
  in
    onKeyDown tag


-- winOnFocus : M.Focus -> Html.Attribute Msg
-- winOnFocus win =
--   let
--     focus height =
--       case win of
--         M.Top -> Many [Focus win, Resize height]
--         M.Bot -> Focus win
--   in
--     Events.on "focus" (Decode.map focus offsetHeight)


winOnFocus : M.Focus -> Html.Attribute Msg
winOnFocus win =
  Events.onFocus (Focus win)


-- | Code below does not work...
-- topOnResize : Html.Attribute Msg
-- topOnResize =
--   Events.on "resize" (Decode.map Resize offsetHeight)


-- | Decode the height of a given element.
offsetHeight : Decode.Decoder Int
offsetHeight =
  Decode.at ["target", "offsetHeight"] Decode.int


---------------------------------------------------
-- Top-level events
---------------------------------------------------


-- onLoadEvent : Html.Attribute Msg
-- onLoadEvent =
--   -- Events.on "load" (Decode.map Resize offsetHeight)
--   -- Events.on "load" (Decode.succeed <| Focus M.Bot 0)
--   -- Events.on "load" (Decode.succeed Next)
--   Events.on "DOMContentLoaded" (Decode.succeed <| Focus M.Bot 0)


---------------------------------------------------
-- Utils
---------------------------------------------------


-- onKeyUp : (Int -> msg) -> Html.Attribute msg
-- onKeyUp tagger =
--   Events.on "keydown" (Decode.map tagger Events.keyCode)


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  Events.on "keydown" (Decode.map tagger Events.keyCode)


px : Int -> String
px number =
  toString number ++ "px"
