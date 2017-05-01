-- import Html exposing (beginnerProgram, div, button, text)
-- import Html exposing (..)
import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg as Svg
import Svg.Attributes as Svg
import Svg.Attributes as SvgAtts


---------------------------------------------------
-- Trees
---------------------------------------------------


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


tree1 : Tree Int
tree1 = Node 1 (Node 2 Empty (Node 5 Empty Empty)) (Node 3 Empty (Node 4 Empty Empty))


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
    , tree : Tree Int
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model (Position 200 200) tree1 Nothing, Cmd.none )



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
  Html.div [] [background, showTree (Position 0 0) model.tree model]
  -- roundRect


-- roundRect : Html.Html msg
-- roundRect =
--   let
--     line = Svg.line
--       [ Svg.stroke "black", Svg.strokeWidth "2"
--       , Svg.x1 "50", Svg.y1 "50", Svg.x2 "200", Svg.y2 "200" ] []
--     -- rect = Svg.rect
--     --   [ Svg.x "10", Svg.y "10", Svg.width "100", Svg.height "100" ] []
--     -- group = Svg.g
--     -- [ ]
--     svg = Svg.svg
--       -- [ Svg.width "1000", Svg.height "1000" ] --, Svg.viewBox "100 100 300 300" ]
--       -- []
--       [ Svg.width "200"
--       , Svg.height "200"
--       ]
--       [ line
--       ]
--   in
--     Html.div
--       [ Atts.style
--           [ "position" => "absolute"
--           , "left" => px 200
--           , "top" => px 200
--           ]
--       ]
--       [svg]


showLine : Position -> Position -> Html.Html Msg
showLine beg end =
  let
    width  = toString <| abs <| end.y - beg.y
    height = toString <| abs <| end.x - beg.x
    (x1, x2) = case end.x >= beg.x of
             True  -> ("0", width)
             False -> (width, "0")
    (y1, y2) = case end.y >= beg.y of
             True  -> ("0", height)
             False -> (height, "0")
    line = Svg.line
      [ Svg.stroke "black", Svg.strokeWidth "2"
      , Svg.x1 x1, Svg.y1 y1, Svg.x2 x2, Svg.y2 y2 ] []
    svg = Svg.svg [Svg.width width, Svg.height height] [line]
  in
    Html.div
      [ Atts.style
          [ "position" => "absolute"
          , "left" => px (min beg.x end.x)
          , "top" => px (min beg.y end.y)
          , "pointer-events" => "none"
          ]
      ]
      [svg]


showTree : Position -> Tree Int -> Model -> Html.Html Msg
showTree pos node model =
  let
    toLeft pos  = {x = pos.x - 100, y = pos.y + 100}
    toRight pos = {x = pos.x + 100, y = pos.y + 100}
  in
    case node of
      Empty -> Html.div [] []
      Node x l r -> Html.div []
        [ circle x pos model
        , showLine
            (getCirclePosition x pos model)
            (getCirclePosition x (toLeft pos) model)
        , showTree (toLeft pos) l model
        , showLine
            (getCirclePosition x pos model)
            (getCirclePosition x (toRight pos) model)
        , showTree (toRight pos) r model
        ]


circle : Int -> Position -> Model -> Html.Html Msg
circle x at model =
  let
    realPosition =
      getCirclePosition x at model
  in
    Html.div
      -- [ onMouseDown
      [ Atts.style
          [ "background-color" => "#3C8D2F"
          -- , "cursor" => "move"

          , "width" => "50px"
          , "height" => "50px"
          , "border-radius" => "50%" -- "4px"
          , "position" => "absolute"
          , "left" => px realPosition.x
          , "top" => px realPosition.y

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ Html.text (toString x) -- "Go!"
      ]


getCirclePosition : Int -> Position -> Model -> Position
getCirclePosition x at model =
  let
    realPosition =
      getPosition model
  in
    { x = at.x + realPosition.x
    , y = at.y + realPosition.y }


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
          [ "opacity" => "0.1"
          , "background-color" => "#ccc"
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
