-- Bounding box


module Edit.View.Box exposing
  ( Box
  , boxLineIntersection

  , Line
  , lineLineIntersection
  )


import Mouse exposing (Position)

import Util as Util


---------------------------------------------------
-- Box
---------------------------------------------------


-- | A bounding box
type alias Box =
    { leftTop : Position
    -- ^ Left-top corner position
    , rightBot : Position
    -- ^ Right-bottom corner position
    }


leftEdge : Box -> Line
leftEdge box =
    { beg = {x = box.leftTop.x, y = box.rightBot.y}
    , end = box.leftTop
    }


topEdge : Box -> Line
topEdge box =
    { beg = box.leftTop
    , end = {x = box.rightBot.x, y = box.leftTop.y}
    }


rightEdge : Box -> Line
rightEdge box =
    { beg = {x = box.rightBot.x, y = box.leftTop.y}
    , end = box.rightBot
    }


bottomEdge : Box -> Line
bottomEdge box =
    { beg = box.rightBot
    , end = {x = box.leftTop.x, y = box.rightBot.y}
    }


boxLineIntersection
    :  Box
    -> Line
    -> Maybe Position
boxLineIntersection box line =
    let
        edges =
            [ leftEdge box
            , topEdge box
            , rightEdge box
            , bottomEdge box
            ]
    in
        Util.mconcat <|
            List.map (lineLineIntersection line) edges


---------------------------------------------------
-- Line
---------------------------------------------------


-- | A line
type alias Line =
    { beg : Position
    , end : Position
    }


-- | An intersection between two lines; the function assumes that the lines are
-- not collinear.
lineLineIntersection : Line -> Line -> Maybe Position
lineLineIntersection linx liny =

    let

        floatPos pos =
            { x = toFloat pos.x
            , y = toFloat pos.y
            }
        convert lin =
            ( floatPos lin.beg
            , minus
                  (floatPos lin.end)
                  (floatPos lin.beg)
            )
        (p, r) = convert linx
        (q, s) = convert liny

        t = cross
            (minus q p)
            (mult (1.0 / cross r s) s)
        u = cross
            (minus p q)
            (mult (1.0 / cross s r) r)

        toPosition v =
            { x = round v.x
            , y = round v.y }
        eps = 0.00000001

    in

        if abs (cross r s) <= eps || t < 0 || t > 1 || u < 0 || u > 1
        then Nothing
        else Just <| toPosition <| add p (mult t r)


---------------------------------------------------
-- Vector
---------------------------------------------------


type alias Vect =
    { x : Float
    , y : Float }


add : Vect -> Vect -> Vect
add v w =
    { x = v.x + w.x
    , y = v.y + w.y
    }


minus : Vect -> Vect -> Vect
minus v w =
    { x = v.x - w.x
    , y = v.y - w.y
    }


mult : Float -> Vect -> Vect
mult k v =
    { x = v.x * k
    , y = v.y * k
    }


-- | Cross product.
cross : Vect -> Vect -> Float
cross v w = v.x*w.y - v.y*w.x
