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
import Mouse exposing (Position)

import Rose as R
import Model as M
import Message as Msg
import Message exposing (Msg(..))
import Config as Cfg



(=>) : a -> b -> (a, b)
(=>) = (,)


-- | View tree in the specified window.
viewTree : M.Window -> M.Model -> Html.Html Msg
viewTree win model =
  let
    selTree = case win of
      M.Top -> case D.get model.topTree model.trees of
        Nothing -> Cfg.testTree1
        Just x  -> x
      M.Bot -> case D.get model.botTree model.trees of
        Nothing -> Cfg.testTree1
        Just x  -> x
    selNodes = case win of
      M.Top -> model.topSelect
      M.Bot -> model.botSelect
  in
    drawTree
      win selNodes
      (M.getPosition win model)
      -- (R.withWidth (\_ -> stdWidth) selTree)
      (R.withWidth Cfg.stdWidth Cfg.stdMargin selTree)


-- | Determine the background color.
backColor : M.Window -> M.Model -> String
backColor win model =
  if win == model.focus
    then "#ddd"
    else "#eee"


viewTreeId : M.Window -> M.Model -> Html.Html Msg
viewTreeId win model =
  let
    txt = toString (M.treePos win model)
       ++ "/"
       ++ toString (M.treeNum model)
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


view : M.Model -> Html.Html Msg
view model =

  let

    top = Html.div
      [ backMouseDown M.Top
      , backOnFocus M.Top

      -- @tabindex required to make the div register keyboard events
      , Atts.attribute "tabindex" "1"
      , backKeyDown

      , Atts.style
        [ "position" => "absolute"
        , "width" => "85%"
        , "height" => "50%"
        , "top" => "0"
        -- overflow is a very important attribute which makes the scrollbars to
        -- appear and which makes sure that the trees do not go beyond the
        -- specified subwindows.
        , "overflow" => "auto"
        , "background-color" => backColor M.Top model
        , "opacity" => "1.0"
        -- z-index important because of its interactions with how the edges are
        -- drawn.
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [ viewTree M.Top model
      , viewTreeId M.Top model ]

    topFS =
      let
        (condAtts, event) = case S.toList model.topSelect of
          [nodeId] ->
            ( [Atts.value (M.getLabel nodeId M.Top model)]
            , ChangeLabel nodeId M.Top )
          _ ->
            ( [Atts.disabled True, Atts.placeholder "<label>"]
            , \_ -> Dummy )
      in
        Html.div
          [ Atts.style
            [ "position" => "absolute"
            , "width" => "15%"
            , "height" => "50%"
            , "right" => "0"
            ]
          ]
          [ Html.input
              -- ( [ Events.onInput (\_ -> Dummy)
              -- ( [ Events.onInput (ChangeLabel M.Top)
              ( [ Events.onInput event
                , Atts.style
                  [ "position" => "absolute"
                  , "width" => "100%"
                  ]
                ] ++ condAtts
              )
              []
          ]

    bottom = Html.div
      [ backMouseDown M.Bot
      , backOnFocus M.Bot

      -- @tabindex required to make the div register keyboard events
      , Atts.attribute "tabindex" "1"
      , backKeyDown

      , Atts.style
        [ "position" => "absolute"
        , "width" => "100%"
        , "height" => "50%"
        , "bottom" => "0"
        , "overflow" => "auto"
        , "background-color" => backColor M.Bot model
        , "opacity" => "1.0"
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [ viewTree M.Bot model
      , viewTreeId M.Bot model ]

  in
    Html.div
      [ Atts.style
          ["width" => "100%", "height" => "100%"]
      ]
      [top, topFS, bottom]


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


drawTree
   : M.Window -- ^ Which window is it in?
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


drawNode : S.Set M.NodeId -> M.Window -> Position -> M.Node -> Html.Html Msg
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


-- drawNode : S.Set M.NodeId -> M.Window -> Position -> M.Node -> Html.Html Msg
-- drawNode select win at node =
--     Html.div
--       [ nodeMouseDown win node
--       , Atts.style
--           [ if S.member node.nodeId select
--               then "background-color" => "#BC0000"
--               else "background-color" => "#3C8D2F"
--           , "cursor" => "pointer"
--
--           , "width" => "50px"
--           , "height" => "50px"
--           , "border-radius" => "50%" -- "4px"
--           , "position" => "absolute"
--           , "left" => px (at.x - nodeWidth // 2)
--           , "top" => px (at.y - nodeHeight // 2)
--
--           , "color" => "white"
--           , "display" => "flex"
--           , "align-items" => "center"
--           , "justify-content" => "center"
--           ]
--       ]
--       [ Html.text node.nodeVal -- "Go!"
--       ]


nodeMouseDown : M.Window -> M.Node -> Html.Attribute Msg
nodeMouseDown win x =
  Events.onMouseDown (Select win x.nodeId)


---------------------------------------------------
-- Background events
---------------------------------------------------


backMouseDown : M.Window -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)


-- backDoubleClick : M.Window -> Html.Attribute Msg
-- backDoubleClick win =
--   Events.onDoubleClick (Focus win)


backOnFocus : M.Window -> Html.Attribute Msg
backOnFocus win =
  Events.onFocus (Focus win)


backKeyDown : Html.Attribute Msg
backKeyDown =
  let
    tag code = case code of
      33 -> Previous
      34 -> Next
      _  -> Dummy
  in
    onKeyDown tag


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


---------------------------------------------------
-- Background
---------------------------------------------------


-- background : Html.Html Msg
-- background =
--     Html.div
--       [ backMouseDown
--       , Atts.style
--           -- thanks to 1.0 opacity and #fff background color,
--           -- drawing artefacts are less visible.
--           [ "opacity" => "1.0"
--           , "background-color" => "#ccc"
--           , "position" => "fixed"
--           , "width" => "100%"
--           , "height" => "100%"
--           , "top" => "0px"
--           , "left" => "0px"
--           , "z-index" => "-1" ]
--       ]
--       []


-- background : Html.Html Msg
-- background =
--     Html.div
--       [ backMouseDown
--       , Atts.style
--           -- thanks to 1.0 opacity and #fff background color,
--           -- drawing artefacts are less visible.
--           [ "opacity" => "1.0"
--           , "background-color" => "#ccc"
--           , "position" => "absolute"
--           , "width" => "100%"
--           , "height" => "100%"
--           , "border" => "1px black solid"
-- --           , "top" => "0px"
-- --           , "left" => "0px"
--           , "z-index" => "-1" ]
--       ]
--       []
--
--
-- backMouseDown : Html.Attribute Msg
-- backMouseDown =
--   Events.on "mousedown" (Decode.map DragStart Mouse.position)
