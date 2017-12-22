module Edit.View.Circle exposing
   ( CircleCfg
   , defCircleCfg
   , drawCircle
   , circleStyle
   , circleStyleExt
   )


import Html as Html
import Html.Attributes as Atts
import Mouse exposing (Position)


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
drawCircle cfg at = Html.div [circleStyle cfg at] []


circleStyle : CircleCfg -> Position -> Html.Attribute msg
circleStyle = circleStyleExt []


-- | Extensible circle style
circleStyleExt
    :  List (String, String) -- ^ Additional style directives
    -> CircleCfg
    -> Position
    -> Html.Attribute msg
circleStyleExt sty cfg at = Atts.style <|
  [ "background-color" :> cfg.color
  , "opacity" :> cfg.opacity
  , "width" :> px cfg.width
  , "height" :> px cfg.height
  , "border-radius" :> "50%"
  , "position" :> "absolute"
  , "left" :> px (at.x - cfg.width // 2)
  , "top" :> px (at.y - cfg.height // 2)
  ] ++ sty


---------------------------------------------------
-- Utils
---------------------------------------------------


(:>) : a -> b -> (a, b)
(:>) = (,)


px : Int -> String
px number =
  toString number ++ "px"
