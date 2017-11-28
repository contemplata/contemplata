module Edit.Init exposing
  ( mkEdit )


import Mouse exposing (Position)
import Window
import Task
import Dom as Dom

import Dict as D
import Set as S

import Config as Cfg
import Edit.Core exposing (FileId, TreeId(..))
import Edit.Model exposing (Model, Focus(..), File, AnnoLevel(..))
import Edit.Message.Core exposing (Msg(..))
import Edit.Message exposing (dummy)
import Edit.Popup as Popup


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkEdit
    : Cfg.Config
    -> FileId
    -> File
    -> (Model, Cmd Msg)
mkEdit config fileId file =
  let
    treeId = case D.toList file.treeMap of
      (id, tree) :: _ -> id
      _ -> Debug.crash "setTrees: empty tree dictionary"
    top = win Top
    bot = win Bot
    win foc =
      { tree = TreeId treeId
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
      , file = file
      , cmpFileId = Nothing
      , cmpFile = Nothing
      -- , trees = file.treeMap
      -- , turns = file.turns
      -- , links = file.linkSet
      , top = top
      , bot = bot
      , focus = Top
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
      , config = config
      , annoLevel = Temporal
      }
    initHeight = Task.perform Resize Window.size
    focusOnTop = Task.attempt
      (\_ -> dummy)
      (Dom.focus <| Cfg.windowName True)
    -- initHeight = Cmd.none
  in
    (model, Cmd.batch [initHeight, focusOnTop])


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
