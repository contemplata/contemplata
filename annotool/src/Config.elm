module Config exposing
  ( Config
  , stdMargin, nodeHeight, moveDown, sideSpace, sideMenuHeight
  -- , testTree1, testTree2, testTree3, testTree4, testTree5
  , increaseSpeed, windowName, editLabelName, selectSentName, splitSelectName
  , sideDivName
  , popupDivTemp
  , dmzSize, linkDasharray, linkWidth, linkOpacity
  , linkCircleOpacity, linkCircleColor, linkCircleRadius
  , linkCircleSelectColor, linkSelectWidth
  , linkHeadSize, linkHeadDist, linkHeadDist2, linkTailDist
  -- , socketServer, socketServerAlt
  -- Main menu
  , menuMaxWidth
  , menuFilesMaxWidth
  , menuHelpMaxWidth
  , menuPadding
  , menuHelpMargin

  -- Relation markers
  , relMarkerOpacity
  , relMarkerSize
  , relMarkerDist

  -- Node colors
  , nodeColor
  , nodeRegularColor
  , nodeTypedColor
  , nodeSelectColor
  , nodeMisplacedColor
  )


import Focus as Lens
import List as L

import Rose as R
import Edit.Core exposing (AnnoName)
-- import Edit.Model as M


---------------------------------------------------
-- Config inherited from the Snap application
---------------------------------------------------


type alias Config =
  { user : AnnoName
  -- ^ User name
  , wsUseProxy : Bool
  -- ^ Use proxy adress for the websocket server
  , socketServer : String
  , socketServerAlt : String
  -- ^ The main and the alternative address of the websocket server
  }


---------------------------------------------------
-- Basic
---------------------------------------------------


-- -- | How far to move left for the next level.
-- moveLeft : Int
-- moveLeft = 100
--
--
-- -- | How far to move right for the next level.
-- moveRight : Int
-- moveRight = 100


-- -- | Standard width of a leaf.
-- stdWidth : Int
-- stdWidth = 100


-- nodeWidth : Int
-- nodeWidth = 50


-- | Horizontal margin before and after the node.
stdMargin : Int
stdMargin = 10
-- stdWidth x = 100


nodeHeight : Int
nodeHeight = 25


-- | How far to move down for the next level.
moveDown : Int
moveDown = 40


-- | Space (proportionally) for side windows.
sideSpace : Int
sideSpace = 15


-- | Height of the side menu.
sideMenuHeight : Int
sideMenuHeight = 25


-- | Size of the demilitarized zone. Use so that scrollbars do
-- not appear.
dmzSize : Int
dmzSize = 10


-- | Space for side windows.
increaseSpeed : Int
increaseSpeed = 5


-- | The name of the edit label field.
editLabelName : Bool -> String
editLabelName flag = case flag of
  True  -> "topEdit"
  False -> "botEdit"


-- | The name of the edit label field.
selectSentName : Bool -> String
selectSentName flag = case flag of
  True  -> "topSelectSent"
  False -> "botSelectSent"


-- | The name of the edit label field.
sideDivName : Bool -> String
sideDivName flag = case flag of
  True  -> "topSideDiv"
  False -> "botSideDiv"


-- | The name of the main editing windows.
windowName : Bool -> String
windowName flag = case flag of
  True  -> "top"
  False -> "bot"


-- | The name of the edit label field.
splitSelectName : String
splitSelectName = "splitSelect"


-- | The name of the edit label field.
popupDivTemp : String
popupDivTemp = "popupDivTemp"


---------------------------------------------------
-- Links
---------------------------------------------------


linkDasharray : String
linkDasharray = "10 10"


linkWidth : Int
linkWidth = 2


linkOpacity : String
linkOpacity = "0.4"


linkHeadSize : Int
linkHeadSize = 15


-- linkHeadDist : (Int, Int)
-- linkHeadDist = (30, 20)


linkHeadDist : Int
linkHeadDist = 15


linkHeadDist2 : Int
linkHeadDist2 = 20


linkTailDist : Int
linkTailDist = 15


linkCircleOpacity : String
linkCircleOpacity = "0.8"


linkCircleColor : String
linkCircleColor = "grey"


linkCircleSelectColor : String
linkCircleSelectColor = "#BC0000"


linkSelectWidth : Int
linkSelectWidth = 3


linkCircleRadius : Int
linkCircleRadius = 25


---------------------------------------------------
-- Relation markers
---------------------------------------------------


relMarkerOpacity : String
relMarkerOpacity = "0.4"


-- Argument tells whether it's the main marker or not.
relMarkerSize : Bool -> Int
relMarkerSize isMain =
    if isMain
    then 15
    else 10


-- | Distance from the node, relative to its width and height
relMarkerDist : Float
relMarkerDist = 1.5


---------------------------------------------------
-- Node colors
---------------------------------------------------


nodeRegularColor : String
nodeRegularColor = "#3C8D2F"


nodeSelectColor : String
nodeSelectColor = "#BC0000"


nodeMisplacedColor : String
nodeMisplacedColor = "#EF597B"


nodeTypedColor : String -> String
nodeTypedColor typ = "#E17000"
--     case typ of
--         "Event"  -> "#CC6600"
--         "Timex"  -> "#18B500"
--         "Signal" -> "#5C00E5"
--         _ -> nodeRegularColor


-- | Node color, based on its type
nodeColor : Maybe String -> String
nodeColor mayTyp =
    Maybe.withDefault nodeRegularColor
        (Maybe.map nodeTypedColor mayTyp)


---------------------------------------------------
-- Test trees
---------------------------------------------------


-- testTree1 : R.Tree M.Node
-- testTree1 =
--   let
--     node i xs = R.Node (M.Node {nodeId = i, nodeVal = toString i}) xs
--     leaf i = R.Node (M.Leaf {nodeId = i, nodeVal = toString i, leafPos = i}) []
--   in
--     node 1
--       [ node 2 [leaf 3]
--       , node 4 [leaf 5]
--       ]
--
--
-- testTree2 : R.Tree M.Node
-- testTree2 =
--   let
--     -- node i xs = R.Node {nodeId = i, nodeVal = toString i} xs
--     node i xs = R.Node (M.Node {nodeId = i, nodeVal = toString i}) xs
--     leaf i = R.Node (M.Leaf {nodeId = i, nodeVal = toString i, leafPos = i}) []
--   in
--     node 1
--       [ node 2 [leaf 3]
--       , node 4 [leaf 5]
--       , leaf 6
--       , node 7
--         [leaf 8, leaf 9, leaf 10]
--       ]
--
--
-- mkSynTree
--    : R.Tree {nodeId : M.NodeId, nodeVal : String}
--   -> R.Tree M.Node
-- mkSynTree (R.Node x ts) =
--   if not (L.isEmpty ts)
--   then R.Node (M.Node x) (L.map mkSynTree ts)
--   else
--     let leaf = M.Leaf
--           { nodeId = x.nodeId
--           , nodeVal = x.nodeVal
--           , leafPos = x.nodeId }
--     in  R.Node leaf []
--
--
-- testTree3 : R.Tree M.Node
-- testTree3 =
--   let
--     node x xs = R.Node x xs
--     tree =
--       node "SENT"
--         [ node "Ssub"
--             [ node "CS" [node "Quand" []]
--             , node "VN"
--                 [ node "CLS" [node "vous" []]
--                 , node "V" [node "savez" []] ]
--             , node "VPinf"
--                 [ node "VN"
--                     [ node "CLO" [node "vous" []]
--                     , node "VINF" [node "venez" []] ]
--                 ]
--             ]
--         , node "PUNC" [node "." []]
--         ]
--     addId i x = (i+1, {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree <| snd <| R.mapAccum addId 1 <| tree
--
--
--
-- testTree4 : R.Tree M.Node
-- testTree4 =
--   let
--     node x xs = R.Node x xs
--     tree =
--       node "SENT"
--         [ node "NP"
--             [ node "NPP" [node "Jean" []] ]
--         , node "VN"
--             [ node "V" [node "est" []]
--             , node "VPP" [node "parti" []] ]
--         , node "PUNC" [node "." []]
--         ]
--     addId i x = (i+1, {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree <| snd <| R.mapAccum addId 1 <| tree
--
--
-- testTree5 : R.Tree M.Node
-- testTree5 =
--   let
--     node x xs = R.Node x xs
--     tree =
--       node "SENT"
--         [ node "ADV" [node "Ensuite" []]
--         , node "PUNC" [node "," []]
--         , node "VN"
--             [ node "CLS" [node "il" []]
--             , node "V" [node "est" []]
--             , node "VPP" [node "allé" []] ]
--         , node "VPinf"
--             [ node "VN" [node "VINF" [node "manger" []]] ]
--         , node "PUNC" [node "." []]
--         ]
--     addId i x = (i+1, {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree <| snd <| R.mapAccum addId 1 <| tree


---------------------------------------------------
-- Server
---------------------------------------------------


-- socketServer : String
-- -- socketServer = "ws://127.0.0.1:8000/ws"
-- socketServer = "ws://vega.info.univ-tours.fr/odil/dev/ws"
--
--
-- -- | An alternatie socket server.
-- socketServerAlt : String
-- -- socketServerAlt = "ws://127.0.0.1:9161"
-- socketServerAlt = "ws://vega.info.univ-tours.fr:16342/ws"
--
--
--
-- -- socketServer = "ws://vega.info.univ-tours.fr/odil/dev/ws"
-- -- -- socketServer = "ws://vega.info.univ-tours.fr:16342/ws"
--
-- -- socketServer = "ws://127.0.0.1:9161"
-- -- socketServer = "ws://vega.info.univ-tours.fr/odil/websocket"
-- -- socketServer = "ws://vega.info.univ-tours.fr/odil/dev/websocket"
-- -- socketServer = "ws://vega.info.univ-tours.fr:16340"




---------------------------------------------------
-- Main menu
---------------------------------------------------


-- | Centering when the width of the page exceeds the given value.
menuMaxWidth : Int
menuMaxWidth = 960


-- | Centering when the width of the page exceeds the given value.
menuFilesMaxWidth : Int
menuFilesMaxWidth = 300


-- | Centering when the width of the page exceeds the given value.
menuHelpMaxWidth : Int
menuHelpMaxWidth = menuMaxWidth - menuFilesMaxWidth -- - menuHelpMargin - menuPadding*2


-- | Padding of the main menu.
menuPadding : Int
menuPadding = 25


-- | Left-margin of the help menu.
menuHelpMargin : Int
menuHelpMargin = 50
