module Edit.Init exposing
  ( mkEdit )


import Mouse exposing (Position)
import Window
import Task

import Dict as D
import Set as S

import Edit.Model exposing (Model, Focus(..), File)
import Edit.Message exposing (Msg(..))


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkEdit : File -> (Model, Cmd Msg)
mkEdit treeDict =
  let
    treeId = case D.toList treeDict of
      (id, tree) :: _ -> id
      _ -> Debug.crash "setTrees: empty tree dictionary"
    top = win treeId
    bot = win treeId
    win name =
      { tree = name
      , pos = Position 400 50
      , selMain = Nothing
      , selAux = S.empty
      , drag = Nothing
      }
    dim =
      { width = 0
      , height = 0
      , heightProp = 50
      }
    model =
      { trees = treeDict
      , top = top
      , bot = bot
      , focus = Top
      , links = S.empty
      , dim = dim
      , ctrl = False
      , testInput = ""
      }
    initHeight = Task.perform Resize Window.size
    -- initHeight = Cmd.none
  in
    (model, initHeight)


-- init : (Model, Cmd Msg)
-- init =
--   let
--     top = win "t1"
--     bot = win "t2"
--     win name =
--       { tree = name
--       , pos = Position 400 50
--       , selMain = Nothing
--       , selAux = S.empty
--       , drag = Nothing
--       }
--     dim =
--       { width = 0
--       , height = 0
--       , heightProp = 50
--       }
--     model =
--       { trees = D.fromList
--           [ ("t1", Cfg.testTree3)
--           , ("t2", Cfg.testTree2)
--           , ("t3", Cfg.testTree1)
--           , ("t4", Cfg.testTree4)
--           , ("t5", Cfg.testTree5)
--           ]
--       , top = top
--       , bot = bot
--       , focus = Top
--       , links = S.fromList
--           [ (("t4", 3), ("t5", 9))
--           , (("t1", 1), ("t1", 2))
--           ]
--       , dim = dim
--       , ctrl = False
--       , testInput = ""
--       }
--     initHeight = Task.perform Resize Window.size
--   in
--     -- (model, Cmd.none)
--     (model, initHeight)
