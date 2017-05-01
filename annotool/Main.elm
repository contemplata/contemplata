-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg as Svg
import Svg.Attributes as Svg

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


-- tree1 : R.Tree Int
-- tree1 = R.Node 1
--   [ R.Node 2 [R.Node 3 []]
--   , R.Node 4 [R.Node 5 []]
--   ]


tree1 : R.Tree Int
tree1 = R.Node 1
  [ R.Node 2 [R.Node 3 []]
  , R.Node 4 [R.Node 5 []]
  , R.Node 6 []
  , R.Node 7
    [R.Node 8 [], R.Node 9 [], R.Node 10 []]
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
    { position : Position
    , tree : R.Tree Int
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model (Position 640 200) tree1 Nothing, Cmd.none )



---------------------------------------------------
-- Update
---------------------------------------------------


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({position, tree, drag} as model) =
  case msg of
    DragStart xy ->
       { position = model.position
       , tree = model.tree
       , drag = Just (Drag xy xy) }

    DragAt xy ->
       { position = model.position
       , tree = model.tree
       , drag = Maybe.map (\{start} -> Drag start xy) drag }

    DragEnd _ ->
       { position = getPosition model
       , tree = model.tree
       , drag = Nothing }



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


view : Model -> Html.Html Msg
view model =
  Html.div []
    [ background
    , drawTree
        (getPosition model)
        (R.withWidth (\_ -> stdWidth) model.tree)
    ]


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


-- drawTree : Position -> R.Tree Int -> Html.Html Msg
-- drawTree pos node =
--   let
--     toLeft pos  = {x = pos.x - moveLeft, y = pos.y + moveDown}
--     toRight pos = {x = pos.x + moveRight, y = pos.y + moveDown}
--   in
--     case node of
--       -- Empty -> Html.div [] []
--       R.Node x [l] -> Html.div []
--         [ drawCircle x pos
--         , drawLine pos (toLeft pos)
--         , drawTree (toLeft pos) l
--         ]
--       R.Node x [l, r] -> Html.div []
--         [ drawCircle x pos
--         , drawLine pos (toLeft pos)
--         , drawTree (toLeft pos) l
--         , drawLine pos (toRight pos)
--         , drawTree (toRight pos) r
--         ]
--       R.Node x _ -> Html.div []
--         [ drawCircle x pos
--         ]


drawTree : Position -> R.Tree (Int, R.Width) -> Html.Html Msg
drawTree pos (R.Node (label, width) subTrees) =
  let
    drawSub w0 forest = case forest of
      [] -> []
      t :: ts ->
        let
          tw = R.getWidth t
          tpos = {x = w0 + tw // 2, y = pos.y + moveDown}
          -- toLeft pos  = {x = pos.x - 50, y = pos.y + moveDown}
        in
          drawTree tpos t :: drawLine pos tpos :: drawSub (w0 + tw) ts
  in
    Html.div []
      (  drawCircle label pos
      :: drawSub (pos.x - width // 2) subTrees )


drawCircle : Int -> Position -> Html.Html Msg
drawCircle x at =
    Html.div
      -- [ onMouseDown
      [ Atts.style
          [ "background-color" => "#3C8D2F"
          -- , "cursor" => "move"

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
      [ Html.text (toString x) -- "Go!"
      ]


-- getCirclePosition : Int -> Position -> Model -> Position
-- getCirclePosition x at model =
--   let
--     realPosition =
--       getPosition model
--   in
--     { x = at.x + realPosition.x
--     , y = at.y + realPosition.y }


px : Int -> String
px number =
  toString number ++ "px"


-- getPosition : Model -> Position
-- getPosition {position, drag} =
--   case drag of
--     Nothing ->
--       position
--     Just {start,current} ->
--       Position
--         (position.x - current.x + start.x)
--         (position.y - current.y + start.y)


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position
    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


-- onMouseDown : Html.Attribute Msg
-- onMouseDown =
--   Events.on "mousedown" (Decode.map DragStart Mouse.position)



---------------------------------------------------
-- Background
---------------------------------------------------


background : Html.Html Msg
background =
    Html.div
      [ onMouseDown
      , Atts.style
          -- thanks to 1.0 opacity and #fff background color,
          -- drawing artefacts are not visible
          [ "opacity" => "1.0"
          , "background-color" => "#fff"
          , "position" => "fixed"
          , "width" => "100%"
          , "height" => "100%"
          , "top" => "0px"
          , "left" => "0px"
          , "z-index" => "-1" ]
      ]
      []


onMouseDown : Html.Attribute Msg
onMouseDown =
  Events.on "mousedown" (Decode.map DragStart Mouse.position)


-- main = beginnerProgram { model = 0, view = view, update = update }
--
--
-- view model =
--   div []
--     [ button [ onClick Decrement ] [ text "-" ]
--     , div [] [ text (toString model) ]
--     , button [ onClick Increment ] [ text "+" ]
--     , button [ onClick Reset ] [ text "RESET" ]
--     ]
--
--
-- type Msg = Increment | Decrement | Reset
--
--
-- update msg model =
--   case msg of
--     Increment ->
--       model + 1
--
--     Decrement ->
--       model - 1
--
--     Reset ->
--       0
