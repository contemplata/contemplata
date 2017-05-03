module Config exposing
  ( stdWidth, stdMargin, nodeHeight, moveDown, sideSpace
  , testTree1, testTree2, testTree3, testTree4, testTree5
  , increaseSpeed, editLabelName )


import Rose as R
import Model as M


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
stdWidth x = max 30 <| (String.length x.nodeVal * 10)
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


-- | Space for side windows.
sideSpace : Int
sideSpace = 15


-- | Space for side windows.
increaseSpeed : Int
increaseSpeed = 5


-- | The name of the edit label field.
editLabelName : Bool -> String
editLabelName flag = case flag of
  True  -> "topEdit"
  False -> "botEdit"


---------------------------------------------------
-- Test trees
---------------------------------------------------


testTree1 : R.Tree M.Node
testTree1 =
  let
    node i xs = R.Node {nodeId = i, nodeVal = toString i} xs
  in
    node 1
      [ node 2 [node 3 []]
      , node 4 [node 5 []]
      ]


testTree2 : R.Tree M.Node
testTree2 =
  let
    node i xs = R.Node {nodeId = i, nodeVal = toString i} xs
  in
    node 1
      [ node 2 [node 3 []]
      , node 4 [node 5 []]
      , node 6 []
      , node 7
        [node 8 [], node 9 [], node 10 []]
      ]


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
    snd <| R.mapAccum addId 1 <| tree



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
    snd <| R.mapAccum addId 1 <| tree


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
    snd <| R.mapAccum addId 1 <| tree
