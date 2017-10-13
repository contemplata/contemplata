{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Odil.Server.Config
(
  serverAddr
, serverPort

-- * DB
-- , dbPath
, dbRegPath
, dbStorePath
, dbAuthPath

-- * Temporary
-- , tempModel
) where


-- import qualified Data.Text as T
import qualified Data.Tree as R
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Odil.Server.Types as V


-----------
-- Core
-----------


-- | The default server address.
serverAddr :: String
-- serverAddr = "127.0.0.1"
serverAddr = "0.0.0.0"


serverPort :: Int
serverPort = 9161


---------------------------------------
-- DB static config
---------------------------------------


-- dbPath :: FilePath
-- dbPath = "/home/kuba/work/odil/bitbucket/backend/data/db-tmp"


dbRegPath :: FilePath
dbRegPath = "reg.json"


dbStorePath :: FilePath
dbStorePath = "store"


dbAuthPath :: FilePath
dbAuthPath = "auth.json"


-- -----------
-- -- Temp
-- -----------
--
--
-- tempModel :: M.Map V.FileId V.File
-- tempModel = M.fromList
--   [ ("file1", file1)
--   , ("file2", file2) ]
--   where
--     file1 = V.File
--       { treeMap = M.fromList [("tree3", testTree3)]
--       , linkSet = S.empty }
--     file2 = V.File
--       { treeMap = M.fromList
--           [ ("tree4", testTree4)
--           , ("tree5", testTree5) ]
--       , linkSet = S.empty }
--
--
-- mkSynTree
--   :: R.Tree V.Node
--   -> R.Tree V.Node
-- mkSynTree (R.Node x@V.Node{..} ts) =
--   if not (null ts)
--   then R.Node x (map mkSynTree ts)
--   else
--     let leaf = V.Leaf
--           { leafId = nodeId
--           , leafVal = nodeVal
--           , leafPos = nodeId }
--     in  R.Node leaf []
--
--
-- testTree3 :: R.Tree V.Node
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
--     addId i x = (i+1, V.Node {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree . snd . L.mapAccumL addId 1 $ tree
--
--
-- testTree4 :: R.Tree V.Node
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
--     -- addId i x = (i+1, {nodeId = i, nodeVal = x})
--     addId i x = (i+1, V.Node {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree . snd . L.mapAccumL addId 1 $ tree
--
--
-- testTree5 :: R.Tree V.Node
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
--     addId i x = (i+1, V.Node {nodeId = i, nodeVal = x})
--     snd (x, y) = y
--   in
--     mkSynTree . snd . L.mapAccumL addId 1 $ tree
