module Edit.View.Link exposing
  (
    viewLinks
  )


import List as L
import Dict as D
import Html as Html
import Html.Events as Events
import Focus as Lens
import Focus exposing ((=>))
import Mouse exposing (Position)

import Rose as R

import Config as Cfg
import Edit.Core as C
import Edit.Model as M
import Edit.Message.Core exposing (Msg(..))
import Edit.View.Circle as Circle
import Edit.View.Tree as Tree


---------------------------------------------------
-- Links
---------------------------------------------------


-- | Only if not in the adjudication mode.
viewLinks
    : M.Model
    -> List (Html.Html Msg)
viewLinks model =
    let
        fileTop = Lens.get (M.top => M.fileId) model
        fileBot = Lens.get (M.bot => M.fileId) model
        linkSet = Lens.get (M.fileLens C.Top => M.linkSet) model
    in
        if fileTop == fileBot
        then L.concatMap
            (viewLink model)
            (D.toList linkSet)
        else []
--     case model.cmpFile of
--         Just _  -> []
--         Nothing ->
--             L.concatMap
--                 (viewLink model)
--                 (D.toList model.mainFile.linkSet)


-- | View the link only if not in the adjudication mode.
viewLink
   : M.Model
  -> (C.Link, M.LinkData)
  -> List (Html.Html Msg)
viewLink model link =
    let
        fileTop = Lens.get (M.top => M.fileId) model
        fileBot = Lens.get (M.bot => M.fileId) model
    in
        if fileTop == fileBot
        then viewLink_ model link
        else []


-- | Internal view link, which does not check that we are not in the
-- adjudication mode.
viewLink_
   : M.Model
  -> (C.Link, M.LinkData)
  -> List (Html.Html Msg)
viewLink_ model ((from, to), linkData) =
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

    trimBot pos =
      if pos.x >= 0 &&
         pos.y >= 0 &&
         pos.x <= mainWidth - Cfg.dmzSize &&
         pos.y <= botHeight - Cfg.dmzSize
      then Just {pos | y = pos.y + topHeight}
      else Nothing

    -- Safe because we know we are not in the adjudication mode.
    getReprId = M.getReprId C.Top

  in

    if
      getReprId top.tree model == Tuple.first from &&
      getReprId bot.tree model == Tuple.first to
    then
      viewLinkDir model (top, bot) (trimTop, trimBot) (from, to, linkData.signalAddr)
--     -- NOTE: The code below is commented out because we want to only draw
--     -- links from the top workspace to the bottom one
--     else if
--       getReprId bot.tree model == Tuple.first from &&
--       getReprId top.tree model == Tuple.first to &&
--       getReprId top.tree model /= getReprId bot.tree model
--     then
--       -- viewLinkDir model (top, bot) (trimTop, trimBot) (from, to, linkData.signalAddr)
--       viewLinkDir model (bot, top) (trimBot, trimTop) (from, to, linkData.signalAddr)
    else
      []


viewLinkDir
   : M.Model
  -> (M.Window, M.Window)
  -> (Position -> Maybe Position, Position -> Maybe Position)
     -- ^ Shifting functions, which calculate the absolute positions for the
     -- corresponding (top/bottom) workspaces (return `Nothing` when they go
     -- beyond their workspaces)
  -> (C.Addr, C.Addr, Maybe C.Addr)
     -- ^ (from, to, maybe signal) addresses
  -> List (Html.Html Msg)
viewLinkDir model (top, bot) (shiftTop, shiftBot) (from, to, signalMay) =
  let

    nodePos1 nodeId pos tree = Tree.nodePos nodeId
      <| Tree.positionTree pos
      <| R.withWidth Tree.stdWidth Cfg.stdMargin tree
    posIn addr foc win shift = Maybe.andThen shift <| nodePos1
      (Tuple.second addr)
      (M.getPosition foc model)
      (M.getTree foc (M.getReprId foc win.tree model) model)

    fromPos =
      if Tuple.first from == M.getReprId C.Top top.tree model
      then posIn from C.Top top shiftTop
      else posIn from C.Bot bot shiftBot
    toPos = -- posIn to bot shiftBot
      if Tuple.first to == M.getReprId C.Top bot.tree model
      then posIn to C.Bot bot shiftBot
      else posIn to C.Top top shiftTop
    signPos = case signalMay of
      Nothing -> Nothing
      Just addr ->
        if Tuple.first addr == M.getReprId C.Top bot.tree model
        then posIn addr C.Bot bot shiftBot
        else posIn addr C.Top top shiftTop

    defLineCfg = Tree.defLineCfg
    lineCfg = { defLineCfg
      | strokeDasharray = Just Cfg.linkDasharray
      , strokeWidth = Cfg.linkWidth
      , opacity = Cfg.linkOpacity }

    defCircleCfg = Circle.defCircleCfg
    circleCfg = { defCircleCfg
      | opacity = Cfg.linkOpacity
      , width = Cfg.linkHeadSize
      , height = Cfg.linkHeadSize }

  in

    case (fromPos, toPos) of
      (Just p, Just q) ->
        let
          trimLine = trimBeg Cfg.linkTailDist << trimEnd Cfg.linkHeadDist
          midCirc = {x = (p.x + q.x) // 2, y = (p.y + q.y) // 2}
          -- endCirc = (trimEnd Cfg.linkHeadDist2 {beg=p, end=q}).end
          lin1 = trimLine <| {beg=p, end=midCirc}
          -- lin2 = trimLine <| {beg=midCirc, end=endCirc}
          lin2 = trimLine <| {beg=midCirc, end=q}
          lin3May = case signPos of
            Nothing -> Nothing
            Just z -> Just <| trimLine <| {beg=midCirc, end=z}
        in
          [ Tree.viewLine lineCfg lin1.beg lin1.end
          , Tree.viewLine lineCfg lin2.beg lin2.end
          -- , Circle.drawCircle circleCfg endCirc
          , drawLinkCircle model (from, to) midCirc ]
          ++ case lin3May of
               Nothing -> []
               Just lin3 -> [Tree.viewLine lineCfg lin3.beg lin3.end]
      _ -> []


-- | Draw the circle which represents the relation.
drawLinkCircle
    : M.Model
    -> C.Link
    -> Position
    -> Html.Html Msg
drawLinkCircle model link at =
  let
    defCircleCfg = Circle.defCircleCfg
    cfg0 = { defCircleCfg
      | opacity = Cfg.linkCircleOpacity
      , color = Cfg.linkCircleColor
      , height = Cfg.linkCircleRadius
      , width = Cfg.linkCircleRadius }
    cfg = if model.selLink == Just link
      then {cfg0 | color = Cfg.linkCircleSelectColor}
      else cfg0
  in
    Html.div
      [ Circle.circleStyle cfg at
      , Events.onClick (SelectLink link)

      -- @tabindex required to make the div register keyboard events
      -- , Atts.attribute "tabindex" "1"
      ]
      []


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
trimEnd : Int -> Vect -> Vect
trimEnd trim ({beg, end} as v) =
  let
    trimProp = toFloat trim / length v
    restProp = 1.0 - trimProp
    x = beg.x + round (toFloat (end.x - beg.x) * restProp)
    y = beg.y + round (toFloat (end.y - beg.y) * restProp)
  in
    {beg=beg, end={x=x, y=y}}


-- | Shorten the beinning of a given vector by a given length.
trimBeg : Int -> Vect -> Vect
trimBeg trim v = inverse <| trimEnd trim <| inverse v
