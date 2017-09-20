module Edit.Init exposing
  ( mkEdit )


import Mouse exposing (Position)
import Window
import Task

import Dict as D
import Set as S

import Edit.Model exposing (Model, Focus(..), File, FileId)
import Edit.Message exposing (Msg(..))
import Edit.Popup as Popup


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkEdit : FileId -> File -> (Model, Cmd Msg)
mkEdit fileId file =
  let
    treeId = case D.toList file.treeMap of
      (id, tree) :: _ -> id
      _ -> Debug.crash "setTrees: empty tree dictionary"
    top = win treeId Top
    bot = win treeId Bot
    win name foc =
      { tree = name
      , pos = Position 400 50
      , selMain = Nothing
      , selAux = S.empty
      , drag = Nothing
      , side = if foc == Top
               then Edit.Model.SideContext
               else Edit.Model.SideLog
      }
    dim =
      { width = 0
      , height = 0
      , widthProp = 80
      , heightProp = 50
      }
    model =
      { fileId = fileId
      , trees = file.treeMap
      , turns = file.turns
      , top = top
      , bot = bot
      , focus = Top
      , links = file.linkSet
      , selLink = Nothing
      , dim = dim
      , ctrl = False
      -- , testInput = ""
      , messages = []
      , undoHist = []
      , redoHist = []
      , undoLast = []
      , command = Nothing
      , popup = Nothing -- Just Popup.Files
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
