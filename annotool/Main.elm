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

import Rose as R


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


-- testTree : R.Tree Int
-- testTree = R.Node 1
--   [ R.Node 2 [R.Node 3 []]
--   , R.Node 4 [R.Node 5 []]
--   ]


testTree : R.Tree Node
testTree =
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


main : Program Never Model Msg
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


type alias Model =
    { tree : R.Tree Node
    , selected : S.Set Id

    , topPos : Position
    , botPos : Position
    , drag : Maybe (Window, Drag)
    }


type alias Id = Int


-- Node in a syntactic tree.
type alias Node = {nodeId : Id, nodeVal : Int}


type alias Drag =
    { start : Position
    , current : Position
    }


-- Windows selector
type Window = Top | Bot


getPosition : Window -> Model -> Position
getPosition win model =
  case (win, model.drag) of
    (Top, Just (Top, {start, current})) ->
      Position
        (model.topPos.x + current.x - start.x)
        (model.topPos.y + current.y - start.y)
    (Top, _) -> model.topPos
    (Bot, Just (Bot, {start, current})) ->
      Position
        (model.botPos.x + current.x - start.x)
        (model.botPos.y + current.y - start.y)
    (Bot, _) -> model.botPos


init : ( Model, Cmd Msg )
init =
  let
    model =
      { tree = testTree
      , selected = S.empty
      , topPos = Position 640 200
      , botPos = Position 640 200
      , drag = Nothing
      }
  in
    (model, Cmd.none)


---------------------------------------------------
-- Update
---------------------------------------------------


type Msg
    = DragStart Window Position
    | DragAt Position
    | DragEnd Position
    | Select Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg model =
  case msg of
    DragStart win xy ->
      { model | drag = Just (win, Drag xy xy) }
    DragAt xy ->
      { model
          | drag = Maybe.map (\(win, {start}) -> (win, Drag start xy)) model.drag
      }
    DragEnd _ ->
      -- { model | drag = Nothing, position = getPosition model }
      { model
          | drag = Nothing
          , topPos = case model.drag of
              Just (Top, _) -> getPosition Top model
              _ -> model.topPos
          , botPos = case model.drag of
              Just (Bot, _) -> getPosition Bot model
              _ -> model.botPos
      }
    Select i ->
      { model | selected =
          case S.member i model.selected of
            True  -> S.remove i model.selected
            False -> S.insert i model.selected }


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Model -> Sub Msg
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


-- view : Model -> Html.Html Msg
-- view model =
--   Html.div []
--     [ background
--     , drawTree
--         model.selected
--         (getPosition model)
--         (R.withWidth (\_ -> stdWidth) model.tree)
--     ]


view : Model -> Html.Html Msg
view model =
  let
    tr win = drawTree
        model.selected
        (getPosition win model)
        (R.withWidth (\_ -> stdWidth) model.tree)
    top = Html.div
      [ backMouseDown Top
      , Atts.style
        [ "position" => "absolute"
        , "width" => "100%"
        , "height" => "50%"
        , "top" => "0"
        -- overflow is a very important attribute which makes the scrollbars to
        -- appear and which makes sure that the trees do not go beyond the
        -- specified subwindows.
        , "overflow" => "auto"
        , "background-color" => "#ddd"
        , "opacity" => "1.0"
        -- z-index important because of its interactions with how the edges are
        -- drawn.
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [tr Top]
    bottom = Html.div
      [ backMouseDown Bot
      , Atts.style
        [ "position" => "absolute"
        , "width" => "100%"
        , "height" => "50%"
        , "bottom" => "0"
        , "overflow" => "auto"
        , "background-color" => "#eee"
        , "opacity" => "1.0"
        , "z-index" => "-1"
        -- , "border" => "1px black solid"
        ]
      ]
      -- [background, tr]
      [tr Bot]
  in
    Html.div
      [Atts.style ["width" => "100%", "height" => "100%"]]
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


drawTree : S.Set Id -> Position -> R.Tree (Node, R.Width) -> Html.Html Msg
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


drawNode : S.Set Id -> Position -> Node -> Html.Html Msg
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


nodeMouseDown : Node -> Html.Attribute Msg
nodeMouseDown x =
  Events.onMouseDown (Select x.nodeId)
  -- Events.on "mousedown" (Decode.map DragStart Mouse.position)


backMouseDown : Window -> Html.Attribute Msg
backMouseDown win =
  Events.on "mousedown" (Decode.map (DragStart win) Mouse.position)



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
