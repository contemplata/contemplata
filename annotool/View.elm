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
import Mouse exposing (Position)

import Rose as R
import Model as M
import Message as Msg
import Message exposing (Msg(..))
import Config as Cfg


(=>) : a -> b -> (a, b)
(=>) = (,)


--
-- viewTree: window (top/bottom) + trees + drag (note that drag could be a part of window)
--

-- | View tree in the specified window.
viewTree : M.Focus -> M.Model -> Html.Html Msg
viewTree focus model =
  let
    -- win = M.focusOn model
    win = case focus of
      M.Top -> model.top
      M.Bot -> model.bot
    selTree = Maybe.withDefault Cfg.testTree1
      <| D.get win.tree model.trees
  in
    drawTree
      focus win.select
      -- (M.getPosition focus model)
      (M.getPosition win)
      (R.withWidth Cfg.stdWidth Cfg.stdMargin selTree)


--
-- backColor: isFocus
--

-- | Determine the background color.
backColor : M.Focus -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"


--
-- viewTreeId: window (M.treePos) + trees (M.treeNum)
--

viewTreeId : M.Focus -> M.Model -> Html.Html Msg
viewTreeId win model =
  let
    txt0 = toString (M.treePos win model)
      ++ "/"
      ++ toString (M.treeNum model)
    txt = txt0 ++ case win of
      M.Top -> " (" ++ toString model.winHeight ++ ")"
      M.Bot -> "" -- " (" ++ toString (model.allHeight - model.topHeight) ++ ")"
  in
    Html.div
      [ Atts.style
        [ "position" => "absolute"
        -- , "width" => "5%"
        -- , "height" => "5%"
        , "top" => px 10
        , "left" => px 10
        ]
      ]
      [ Html.text txt ]


--
-- viewTree: window + isFocus + trees + drag + ..?
--

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
        M.Top -> toString model.winProp ++ "%"
        M.Bot -> toString (100 - model.winProp) ++ "%"
    , case win of
        M.Top -> "top" => "0"
        M.Bot -> "bottom" => "0"
    -- overflow is a very important attribute which makes the scrollbars to
    -- appear and which makes sure that the trees do not go beyond the
    -- specified subwindows.
    , "overflow" => "auto"
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
            M.Top -> toString model.winProp ++ "%"
            M.Bot -> toString (100 - model.winProp) ++ "%"
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


viewLink : M.Model -> (M.Addr, M.Addr) -> List (Html.Html Msg)
viewLink model (from, to) =
  if not ( model.top.tree == Tuple.first from
        && model.bot.tree == Tuple.first to )
  then []
  else
    let
      -- toPos (v, w) = {x=v, y=w}
      nodePos1 nodeId pos tree = nodePos nodeId pos
        (R.withWidth Cfg.stdWidth Cfg.stdMargin tree)
      begPos = Maybe.andThen
        (nodePos1 (Tuple.second from) model.top.pos)
        (D.get model.top.tree model.trees)
      endPos0 = Maybe.andThen
        (nodePos1 (Tuple.second to) model.bot.pos)
        (D.get model.bot.tree model.trees)
      topHeight = (model.winHeight * model.winProp) // 100
      endPos = Maybe.map
        (\pos -> {pos | y = pos.y + topHeight})
        endPos0
--       shift pos1 pos2 =
--         { x = pos1.x + pos2.x
--         , y = pos1.y + pos2.y }
    in
      case (begPos, endPos) of
        (Just p, Just q) ->
          [ drawLine p q
--               (shift model.topPos p)
--               (shift model.botPos q)
          ]
        _ -> []


view : M.Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        ["width" => "100%", "height" => "100%"]
    ]
    ( [ viewWindow M.Top model, viewSideWindow M.Top model
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


-- | Retrieve the position of a node in a given tree.
nodePos : M.NodeId -> Position -> R.Tree (M.Node, R.Width) -> Maybe Position
nodePos nodeId pos (R.Node (node, rootWidth) subTrees) =
  let
    forestWidth = List.sum <| L.map R.getWidth subTrees
    nodePosF w0 forest = case forest of
      [] -> Nothing
      t :: ts ->
        let
          tw = R.getWidth t
          tpos = {x = w0 + tw // 2, y = pos.y + Cfg.moveDown}
        in
          case nodePos nodeId tpos t of
            Nothing -> nodePosF (w0 + tw) ts
            Just x  -> Just x
  in
    if nodeId == node.nodeId
    then Just pos
    else nodePosF (pos.x - forestWidth // 2) subTrees


drawTree
   : M.Focus -- ^ Which window is it in?
  -> S.Set M.NodeId -- ^ Selected nodes
  -> Position -- ^ Start position
  -> R.Tree (M.Node, R.Width) -- ^ Tree to draw
  -> Html.Html Msg
drawTree win select pos (R.Node (node, _) subTrees) =
  let
    forestWidth = List.sum <| L.map R.getWidth subTrees
    drawSub w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = R.getWidth t
          tpos = {x = w0 + tw // 2, y = pos.y + Cfg.moveDown}
        in
          drawTree win select tpos t
            :: drawLine pos tpos
            :: drawSub (w0 + tw) ts
  in
    Html.div []
      (  drawNode select win pos node
      :: drawSub (pos.x - forestWidth // 2) subTrees )


drawNode : S.Set M.NodeId -> M.Focus -> Position -> M.Node -> Html.Html Msg
drawNode select win at node =
  let
    -- width = nodeWidth
    width = Cfg.stdWidth node
    height = Cfg.nodeHeight
  in
    Html.div
      [ nodeMouseDown win node
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
