-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg as Svg
import Svg.Attributes as Svg
import Set as S
import Dict as D

import Rose as R
-- import Model exposing (Model)
import Model as M


---------------------------------------------------
-- Config
---------------------------------------------------


-- -- | How far to move left for the next level.
-- moveLeft : Int
-- moveLeft = 100
--
--
-- -- | How far to move right for the next level.
-- moveRight : Int
-- moveRight = 100


-- | Standard width of a leaf.
stdWidth : Int
stdWidth = 100


-- | How far to move down for the next level.
moveDown : Int
moveDown = 100


circleWidth : Int
circleWidth = 50


circleHeight : Int
circleHeight = 50


---------------------------------------------------
-- Test trees
---------------------------------------------------


testTree1 : R.Tree M.Node
testTree1 =
  let
    node i xs = R.Node {nodeId = i, nodeVal = i} xs
  in
    node 1
      [ node 2 [node 3 []]
      , node 4 [node 5 []]
      ]


testTree2 : R.Tree M.Node
testTree2 =
  let
    node i xs = R.Node {nodeId = i, nodeVal = i} xs
  in
    node 1
      [ node 2 [node 3 []]
      , node 4 [node 5 []]
      , node 6 []
      , node 7
        [node 8 [], node 9 [], node 10 []]
      ]


---------------------------------------------------
-- Main
---------------------------------------------------


main : Program Never M.Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


---------------------------------------------------
-- Model
---------------------------------------------------


init : ( M.Model, Cmd Msg )
init =
  let
    model =
      { trees = D.fromList
          [ ("t1", testTree1)
          , ("t2", testTree2)
          ]
      , topTree = "t1"
      , botTree = "t2"
      , selected = S.empty
      , topPos = Position 640 200
      , botPos = Position 640 200
      , drag = Nothing
      , focus = M.Top
      }
  in
    (model, Cmd.none)


---------------------------------------------------
-- Update
---------------------------------------------------


type Msg
    = DragStart M.Window Position
    | DragAt Position
    | DragEnd Position
    | Select M.NodeId
    | Focus M.Window
    | KeyDown Int
--     | Previous
--     | Next
--     | Dummy


update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> M.Model -> M.Model
updateHelp msg model =
  case msg of
    DragStart win xy ->
      { model | drag = Just (win, M.Drag xy xy) }
    DragAt xy ->
      { model
          | drag = Maybe.map (\(win, {start}) -> (win, M.Drag start xy)) model.drag
      }
    DragEnd _ ->
      -- { model | drag = Nothing, position = M.getPosition model }
      { model
          | drag = Nothing
          , topPos = case model.drag of
              Just (M.Top, _) -> M.getPosition M.Top model
              _ -> model.topPos
          , botPos = case model.drag of
              Just (M.Bot, _) -> M.getPosition M.Bot model
              _ -> model.botPos
      }
    Select i ->
      { model | selected =
          case S.member i model.selected of
            True  -> S.remove i model.selected
            False -> S.insert i model.selected }
    Focus win ->
      { model | focus = win }
    KeyDown key -> case key of
      33 -> { model | focus = M.Top } -- Previous
      34 -> { model | focus = M.Bot } -- Next
      _  -> { model | focus = M.Top }
--     Next ->
--       { model | topTree = M.nextTree model.topTree model }
--     Previous -> model
--     Dummy -> model -- it would be better to avoid this...


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : M.Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


---------------------------------------------------
-- View
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)


-- view : M.Model -> Html.Html Msg
-- view model =
--   Html.div []
--     [ background
--     , drawTree
--         model.selected
--         (M.getPosition model)
--         (R.withWidth (\_ -> stdWidth) model.tree)
--     ]


view : M.Model -> Html.Html Msg
view model =
  let
    tr win =
      let
        selTree = case win of
          M.Top -> case D.get model.topTree model.trees of
            Nothing -> testTree1
            Just x  -> x
          M.Bot -> case D.get model.botTree model.trees of
            Nothing -> testTree1
            Just x  -> x
      in
        drawTree
          model.selected
          (M.getPosition win model)
          (R.withWidth (\_ -> stdWidth) selTree)
    backColor win =
      if win == model.focus
        then "#ddd"
        else "#eee"
    top = Html.div
      [ backMouseDown M.Top
      , backDoubleClick M.Top

      -- @tabindex required to make the div register keyboard events
      , Atts.attribute "tabindex" "1"
      , onKeyDown KeyDown

      , Atts.style
        [ "position" => "absolute"
        , "width" => "100%"
        , "height" => "50%"
        , "top" => "0"
        -- overflow is a very important attribute which makes the scrollbars to
        -- appear and which makes sure that the trees do not go beyond the
        -- specified subwindows.
        , "overflow" => "auto"
        , "background-color" => backColor M.Top
        , "opacity" => "1.0"
        -- z-index important because of its interactions with how the edges are
        -- drawn.
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [tr M.Top]
    bottom = Html.div
      [ backMouseDown M.Bot
      , backDoubleClick M.Bot

      -- @tabindex required to make the div register keyboard events
      , Atts.attribute "tabindex" "1"
      , onKeyDown KeyDown

      , Atts.style
        [ "position" => "absolute"
        , "width" => "100%"
        , "height" => "50%"
        , "bottom" => "0"
        , "overflow" => "auto"
        , "background-color" => backColor M.Bot
        , "opacity" => "1.0"
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [tr M.Bot]
  in
    Html.div
      [ Atts.style
          ["width" => "100%", "height" => "100%"]
      ]
      [top, bottom]


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


drawTree : S.Set M.NodeId -> Position -> R.Tree (M.Node, R.Width) -> Html.Html Msg
drawTree select pos (R.Node (node, width) subTrees) =
  let
    drawSub w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = R.getWidth t
          tpos = {x = w0 + tw // 2, y = pos.y + moveDown}
          -- toLeft pos  = {x = pos.x - 50, y = pos.y + moveDown}
        in
          drawTree select tpos t :: drawLine pos tpos :: drawSub (w0 + tw) ts
  in
    Html.div []
      (  drawNode select pos node
      :: drawSub (pos.x - width // 2) subTrees )


drawNode : S.Set M.NodeId -> Position -> M.Node -> Html.Html Msg
drawNode select at node =
    Html.div
      [ nodeMouseDown node
      , Atts.style
          [ if S.member node.nodeId select
              then "background-color" => "#BC0000"
              else "background-color" => "#3C8D2F"
          , "cursor" => "pointer"

          , "width" => "50px"
          , "height" => "50px"
          , "border-radius" => "50%" -- "4px"
          , "position" => "absolute"
          , "left" => px (at.x - circleWidth // 2)
          , "top" => px (at.y - circleHeight // 2)

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ Html.text (toString node.nodeVal) -- "Go!"
      ]


px : Int -> String
px number =
  toString number ++ "px"


nodeMouseDown : M.Node -> Html.Attribute Msg
nodeMouseDown x =
  Events.onMouseDown (Select x.nodeId)


---------------------------------------------------
-- Background events
---------------------------------------------------


backMouseDown : M.Window -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)


backDoubleClick : M.Window -> Html.Attribute Msg
backDoubleClick win =
  Events.onDoubleClick (Focus win)


---------------------------------------------------
-- Top-level events
---------------------------------------------------


-- keyUp : Html.Attribute Msg
-- keyUp =
--   let
--     tag code = case code of
--       33 -> Focus M.Top -- Previous
--       34 -> Focus M.Bot -- Next
--       _  -> Focus M.Top
--   in
--     onKeyUp tag


---------------------------------------------------
-- Utils
---------------------------------------------------


-- onKeyUp : (Int -> msg) -> Html.Attribute msg
-- onKeyUp tagger =
--   Events.on "keydown" (Decode.map tagger Events.keyCode)


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  Events.on "keydown" (Decode.map tagger Events.keyCode)


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
