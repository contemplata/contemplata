module Edit.View.Link exposing
  (
    viewLinks
  )


import List as L
import Dict as D
import Html as Html
import Html.Events as Events
import Html.Attributes as Atts
import Focus as Lens
import Focus exposing ((=>))
import Mouse exposing (Position)

import Rose as R

import Config as Cfg
import Edit.Core as C
import Edit.Model as M
import Edit.Anno as Anno
import Edit.Anno.Core as Anno
import Edit.Message.Core exposing (Msg(..))
import Edit.View.Circle as Circle
import Edit.View.Tree as Tree


---------------------------------------------------
-- Links
---------------------------------------------------


-- | A link.
type LinkTyp
    = Primary
      -- ^ Primary link, with the corresponding annotation
    | Secondary
      -- ^ Secondary link (representing an anchor, for example)


-- | Only if not in the adjudication mode.
viewLinks
    : M.Model
    -> List (Html.Html Msg)
viewLinks model =
    let
        fileTop = Lens.get (M.top => M.fileId) model
        fileBot = Lens.get (M.bot => M.fileId) model
        linkMap = Lens.get (M.fileLens C.Top => M.linkMap) model
        primary = L.concatMap (viewLink model Primary) (D.keys linkMap)
        secondary = L.concatMap (viewLink model Secondary) (getSecondaryLinks model)
    in
        if fileTop == fileBot
        then primary ++ secondary
        else []


-- | View the given link (only if not in the adjudication mode).
viewLink : M.Model -> LinkTyp -> C.Link -> List (Html.Html Msg)
viewLink model linkTyp (from, to) =
  let
    top = Lens.get (M.windowLens C.Top) model
    bot = Lens.get (M.windowLens C.Bot) model
    dim = model.dim

    mainWidth = (dim.width * dim.widthProp) // 100
    topHeight = (dim.height * dim.heightProp) // 100
    botHeight = dim.height - topHeight

    trimTop pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= topHeight - Cfg.dmzSize
      then Just pos
      else Nothing
    posInTop addr = posIn addr C.Top trimTop model

    trimBot pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= botHeight - Cfg.dmzSize
      then Just {pos | y = pos.y + topHeight}
      else Nothing
    posInBot addr = posIn addr C.Bot trimBot model

    -- Safe because we know we are not in the adjudication mode.
    getReprId = M.getReprId C.Top

    -- Select the link viewing function depending on the link's type
    doViewLink =
      case linkTyp of
        Primary -> viewLinkDir
        Secondary -> viewSecLink
  in
    if
      getReprId top.tree model == Tuple.first from &&
      getReprId bot.tree model == Tuple.first to
    then
      doViewLink model (posInTop, posInBot) (from, to)
    else
      []


-- | View a directed relation, together with the corresponding anchors (i.e.
-- secondary relations).
viewLinkDir
   : M.Model
  -> (C.Addr -> Maybe Position, C.Addr -> Maybe Position)
     -- ^ Shifting functions, which calculate the absolute positions for the
     -- corresponding (top/bottom) node addresses (return `Nothing` when they go
     -- beyond their workspaces)
  -> (C.Addr, C.Addr)
     -- ^ (from, to) addresses
  -> List (Html.Html Msg)
viewLinkDir model (posInTop, posInBot) (from, to) =
  let
    defLineCfg = Tree.defLineCfg
    lineCfg0 = { defLineCfg
      | strokeDasharray = Just Cfg.linkDasharray
      , strokeWidth = Cfg.linkWidth
      , opacity = Cfg.linkOpacity }
    lineCfg =
        if model.selLink == Just (from, to)
        then
            { lineCfg0
                | color = Cfg.linkCircleSelectColor
                , strokeWidth = Cfg.linkSelectWidth
            }
        else lineCfg0
    secLineCfg =
        {lineCfg | strokeDasharray = Just Cfg.secLinkDasharray}

    defCircleCfg = Circle.defCircleCfg
    circleCfg = { defCircleCfg
      | opacity = Cfg.linkOpacity
      , width = Cfg.linkHeadSize
      , height = Cfg.linkHeadSize }

    -- Position of an anchor
    anchorPos addr =
      let
        (treeId, _) = addr
        getTreeId foc = M.getReprId foc (M.selectWin foc model).tree model
      in
        if treeId == getTreeId C.Bot
        then posInBot addr
        else if treeId == getTreeId C.Top
             then posInTop addr
             else Nothing

  in
    case (posInTop from, posInBot to) of
      (Just p, Just q) ->
        let
          trimLine =
              trimBeg (toFloat Cfg.linkTailDist) <<
              trimEnd (toFloat Cfg.linkHeadDist)
          midCirc = {x = (p.x + q.x) // 2, y = (p.y + q.y) // 2}
          lin1 = trimLine <| {beg=p, end=midCirc}
          lin2 = trimLine <| {beg=midCirc, end=q}
          anchors =
              let file = Lens.get (M.fileLens C.Top) model in
              case D.get (from, to) file.linkMap of
                  Nothing -> []
                  Just ent -> entityAnchors ent
          lin3Many =
              let process z = trimLine <| {beg=midCirc, end=z}
              in  L.filterMap (Maybe.map process << anchorPos) anchors
        in
          [ Tree.viewLine lineCfg lin1.beg lin1.end
          , drawLinkCircle model (from, to) midCirc ]
          ++ viewDirLine lineCfg lin2.beg lin2.end
          ++ List.concatMap (\lin3 -> viewDirLine secLineCfg lin3.beg lin3.end) lin3Many
      _ -> []


-- | View a secondary link between two nodes. In terms of code, it's a
-- simplified version of `viewLinkDir`.
viewSecLink
   : M.Model
  -> (C.Addr -> Maybe Position, C.Addr -> Maybe Position)
     -- ^ Shifting functions, which calculate the absolute positions for the
     -- corresponding (top/bottom) node addresses (return `Nothing` when they go
     -- beyond their workspaces)
  -> (C.Addr, C.Addr)
     -- ^ (from, to) addresses
  -> List (Html.Html Msg)
viewSecLink model (posInTop, posInBot) (from, to) =
  let
    defLineCfg = Tree.defLineCfg
    lineCfg0 = { defLineCfg
      | strokeDasharray = Just Cfg.secLinkDasharray
      , strokeWidth = Cfg.linkWidth
      , opacity = Cfg.linkOpacity }
    lineCfg =
        if model.selLink == Just (from, to)
        then
            { lineCfg0
                | color = Cfg.linkCircleSelectColor
                , strokeWidth = Cfg.linkSelectWidth
            }
        else lineCfg0

    defCircleCfg = Circle.defCircleCfg
    circleCfg = { defCircleCfg
      | opacity = Cfg.linkOpacity
      , width = Cfg.linkHeadSize
      , height = Cfg.linkHeadSize }

  in
    case (posInTop from, posInBot to) of
      (Just p, Just q) -> viewDirLine lineCfg p q
      _ -> []


-- | Draw the circle which represents the relation.
drawLinkCircle
    :  M.Model
    -> C.Link
    -> Position
    -> Html.Html Msg
drawLinkCircle model link at =
  let
    linkTyp =
        case D.get link (Lens.get (M.fileLens C.Top => M.linkMap) model) of
            Nothing -> "???"
            Just ent -> ent.name
    width = max 30 <| String.length linkTyp * 11
    height = Cfg.linkCircleHeight
    nodeColor =
        if model.selLink == Just link
        then Cfg.linkCircleSelectColor
        else Cfg.linkCircleColor

  in
    Html.div
      -- See also `Edit.View.Tree.drawInternal`
      [ Events.onClick (SelectLink link)
      , Atts.class "noselect"
      , Atts.style
          [ "cursor" :> "pointer"
          , "background-color" :> nodeColor
          , "border" :> "none"

          , "width" :> px width
          , "height" :> px height
          , "border-radius" :> "40%" -- "4px"
          , "position" :> "absolute"

          , "left" :> px (at.x - width // 2)
          , "top" :> px (at.y - height // 2)

          , "color" :> "white"
          , "display" :> "flex"
          , "align-items" :> "center"
          , "justify-content" :> "center"
          ]
      ]
      [Html.p [] [Html.text linkTyp]]


-- | Draw a directed line.
viewDirLine lineCfg p q =
    let
        shorten v = trimEnd (length v - toFloat Cfg.linkArrowSize) v
        c1 = shorten <| rotate Cfg.linkArrowAngle    {beg=q, end=p}
        c2 = shorten <| rotate (-Cfg.linkArrowAngle) {beg=q, end=p}
    in
        [ Tree.viewLine lineCfg p q
        , Tree.viewLine lineCfg c1.beg c1.end
        , Tree.viewLine lineCfg c2.beg c2.end
        ]


---------------------------------------------------
-- Link utilities
---------------------------------------------------


-- | The position of the root?
nodePos1 : C.NodeId -> Position -> R.Tree M.Node -> Maybe Position
nodePos1 nodeId pos tree = Tree.nodePos nodeId
  <| Tree.positionTree pos
  <| R.withWidth Tree.stdWidth Cfg.stdMargin tree


-- | Compute the position of the node with the given address.
posIn
    :  C.Addr    -- ^ The address in question
    -> C.Focus   -- ^ The corresponding focus
    -> (Position -> Maybe Position)
                 -- ^ The shifting function (see e.g. `viewLinkDir`)
    -> M.Model   -- ^ The model
    -> Maybe Position
posIn addr foc shift model =
    let
        win = M.selectWin foc model
    in
        Maybe.andThen shift <| nodePos1
            (Tuple.second addr)
            (M.getPosition foc model)
            (M.getTree foc (M.getReprId foc win.tree model) model)


---------------------------------------------------
-- Secondary links
---------------------------------------------------


-- | Obtain the list of secondary (i.e., anchor related) links stretching
-- between the trees in focus.
getSecondaryLinks : M.Model -> List C.Link
getSecondaryLinks model =
    let
        getTreeId foc = M.getReprId foc (M.selectWin foc model).tree model

        -- Tree IDs
        topTreeId = getTreeId C.Top
        botTreeId = getTreeId C.Bot

        -- The trees themselves
        topTree = M.getTree C.Top topTreeId model
        -- botTree = M.getTree C.Bot botTreeId model

        -- Get the list of the attributes of the given node
        nodeAtts node =
            let nodeId = Lens.get M.nodeId node in
            L.map (\atts -> (nodeId, atts)) <|
                case node of
                    M.Leaf _ -> []
                    M.Node r ->
                        Maybe.withDefault [] <|
                        Maybe.map (D.values << .attributes) r.nodeTyp
        getLink (topNodeId, attr) =
            case attr of
                Anno.Anchor addr ->
                    Just ((topTreeId, topNodeId), addr)
                _ -> Nothing

        -- Does the relation ends in the bottom window?
        endsOnBottom (_, (toTreeId, _)) = toTreeId == botTreeId
    in
        R.flatten topTree |>
        L.concatMap nodeAtts |>
        L.filterMap getLink |>
        L.filter endsOnBottom


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
trimEnd : Float -> Vect -> Vect
trimEnd trim ({beg, end} as v) =
  let
    trimProp = trim / length v
    restProp = 1.0 - trimProp
    x = beg.x + round (toFloat (end.x - beg.x) * restProp)
    y = beg.y + round (toFloat (end.y - beg.y) * restProp)
  in
    {beg=beg, end={x=x, y=y}}


-- | Shorten the beinning of a given vector by a given length.
trimBeg : Float -> Vect -> Vect
trimBeg trim v = inverse <| trimEnd trim <| inverse v


-- | Rotate the given vector by the given angle.
rotate : Float -> Vect -> Vect
rotate th v =
    let
        x = toFloat (v.end.x - v.beg.x)
        y = toFloat (v.end.y - v.beg.y)
        xp = (x * cos th) - (y * sin th)
        yp = (x * sin th) + (y * cos th)
    in
        { beg = v.beg
        , end =
            { x = v.beg.x + round xp
            , y = v.beg.y + round yp
            }
        }


---------------------------------------------------
-- Utils
---------------------------------------------------


(:>) : a -> b -> (a, b)
(:>) = (,)


px : Int -> String
px number =
  toString number ++ "px"


-- | Retrieve all the anchors of the given annotation entity.
entityAnchors : Anno.Entity -> List C.Addr
entityAnchors ent =
    let
        extractAnchor attr =
            case attr of
                Anno.Anchor addr -> Just addr
                _ -> Nothing
    in
        L.filterMap extractAnchor
            <| D.values ent.attributes
