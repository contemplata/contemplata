module Config exposing
  ( stdWidth, stdMargin, nodeHeight, moveDown, sideSpace, sideMenuHeight
  , testTree1, testTree2, testTree3, testTree4, testTree5
  , increaseSpeed, editLabelName, dmzSize
  , linkDasharray, linkWidth, linkOpacity
  , linkHeadSize, linkHeadDist, linkTailDist
  , socketServer
  )


import Focus as Lens
import List as L

import Rose as R
import Edit.Model as M


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


-- | Width of a node.
stdWidth : M.Node -> Int
stdWidth x =
  -- let val = Lens.get M.nodeVal x
  let
    (txt, ix) = case x of
      M.Node r -> (r.nodeVal, "")
      M.Leaf r -> (r.nodeVal, toString r.leafPos)
  in  max 30 <| String.length txt * 10 + String.length ix * 6
-- stdWidth x = 100

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


linkHeadDist : (Int, Int)
linkHeadDist = (30, 20)


linkTailDist : Int
linkTailDist = 15


---------------------------------------------------
-- Test trees
---------------------------------------------------


testTree1 : R.Tree M.Node
testTree1 =
  let
    node i xs = R.Node (M.Node {nodeId = i, nodeVal = toString i}) xs
    leaf i = R.Node (M.Leaf {nodeId = i, nodeVal = toString i, leafPos = i}) []
  in
    node 1
      [ node 2 [leaf 3]
      , node 4 [leaf 5]
      ]


testTree2 : R.Tree M.Node
testTree2 =
  let
    -- node i xs = R.Node {nodeId = i, nodeVal = toString i} xs
    node i xs = R.Node (M.Node {nodeId = i, nodeVal = toString i}) xs
    leaf i = R.Node (M.Leaf {nodeId = i, nodeVal = toString i, leafPos = i}) []
  in
    node 1
      [ node 2 [leaf 3]
      , node 4 [leaf 5]
      , leaf 6
      , node 7
        [leaf 8, leaf 9, leaf 10]
      ]


mkSynTree
   : R.Tree {nodeId : M.NodeId, nodeVal : String}
  -> R.Tree M.Node
mkSynTree (R.Node x ts) =
  if not (L.isEmpty ts)
  then R.Node (M.Node x) (L.map mkSynTree ts)
  else
    let leaf = M.Leaf
          { nodeId = x.nodeId
          , nodeVal = x.nodeVal
          , leafPos = x.nodeId }
    in  R.Node leaf []


testTree3 : R.Tree M.Node
testTree3 =
  let
    node x xs = R.Node x xs
    tree =
      node "SENT"
        [ node "Ssub"
            [ node "CS" [node "Quand" []]
            , node "VN"
                [ node "CLS" [node "vous" []]
                , node "V" [node "savez" []] ]
            , node "VPinf"
                [ node "VN"
                    [ node "CLO" [node "vous" []]
                    , node "VINF" [node "venez" []] ]
                ]
            ]
        , node "PUNC" [node "." []]
        ]
    addId i x = (i+1, {nodeId = i, nodeVal = x})
    snd (x, y) = y
  in
    mkSynTree <| snd <| R.mapAccum addId 1 <| tree



testTree4 : R.Tree M.Node
testTree4 =
  let
    node x xs = R.Node x xs
    tree =
      node "SENT"
        [ node "NP"
            [ node "NPP" [node "Jean" []] ]
        , node "VN"
            [ node "V" [node "est" []]
            , node "VPP" [node "parti" []] ]
        , node "PUNC" [node "." []]
        ]
    addId i x = (i+1, {nodeId = i, nodeVal = x})
    snd (x, y) = y
  in
    mkSynTree <| snd <| R.mapAccum addId 1 <| tree


testTree5 : R.Tree M.Node
testTree5 =
  let
    node x xs = R.Node x xs
    tree =
      node "SENT"
        [ node "ADV" [node "Ensuite" []]
        , node "PUNC" [node "," []]
        , node "VN"
            [ node "CLS" [node "il" []]
            , node "V" [node "est" []]
            , node "VPP" [node "allé" []] ]
        , node "VPinf"
            [ node "VN" [node "VINF" [node "manger" []]] ]
        , node "PUNC" [node "." []]
        ]
    addId i x = (i+1, {nodeId = i, nodeVal = x})
    snd (x, y) = y
  in
    mkSynTree <| snd <| R.mapAccum addId 1 <| tree


---------------------------------------------------
-- Server
---------------------------------------------------


socketServer : String
socketServer = "ws://127.0.0.1:9161"
-- socketServer = "ws://vega.info.univ-tours.fr:16340"
