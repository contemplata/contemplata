module Edit.Message.Core exposing
    ( Msg(..)
    )


import Mouse exposing (Position)
import Window as Window

import Edit.Model as M
import Edit.Core as C
import Edit.Anno as Anno
import Edit.Popup as Popup
import Server


type Msg
  = DragStart M.Focus Position
    -- ^ Neither `DragAt` nor `DragEnd` have their focus. This is on purpose.
    -- Focus should be determined, in their case, on the basis of the drag in
    -- the underlying model. We do not support concurrent drags at the moment.
  | DragAt Position
  | DragEnd Position
  | Select M.Focus C.NodeId
  | SelectTree M.Focus C.PartId
    -- ^ Select tree (or join if with CTRL);
  | SelectToken M.Focus C.PartId Int
    -- ^ Select token; the last argument represents the token ID.
  | SelectLink M.Link
  | Focus M.Focus
  | Resize Window.Size -- ^ The height and width of the entire window
  | Increase Bool Bool -- ^ Change the proportions of the window
  | Previous
  | Next
  | ChangeLabel C.NodeId M.Focus String
  | EditLabel
  | Delete -- ^ Delete the selected nodes in the focused window
  | DeleteTree
    -- ^ Delete the selected nodes in the focused window
    -- together with the corresponding subtrees
  | Add -- ^ Delete the selected nodes in the focused window
  -- | ChangeType -- ^ Change the type of the selected node
  | MkSignal -- ^ Create signal
  | MkEvent -- ^ Create event
  | MkTimex -- ^ Create event
  | ParseRaw Bool  -- ^ Reparse from scratch the sentence in focus; the argument determines
                   -- wheter pre-processing should be used or not
  | ParseSent Server.ParserTyp  -- ^ Reparse the sentence in focus
--   | ParseSentPos Server.ParserTyp -- ^ Reparse the sentence in focus, preserve POList (String, String)S tags
  | ParseSentPos Server.ParserTyp -- ^ Reparse the selected sub-sentence(s) in focus, preserve the POS tags
  | ParseSentCons Server.ParserTyp  -- ^ Reparse the sentence in focus with the selected nodes as constraints
  | ApplyRules -- ^ Apply the (flattening) rules
  | CtrlDown
  | CtrlUp
  | Connect
  | Attach
  | Swap Bool
  | Files -- ^ Go back to files menu
  | SaveFile  -- ^ Save the current file
  | SplitTree  -- ^ Split the tree
  | Join  -- ^ Merge the two trees in view
  | ConcatWords  -- ^ Merge two (or more) words
  -- | Break -- ^ Break the given partition into its components
  | Undo
  | Redo
  | SideMenuEdit M.Focus
  | SideMenuContext M.Focus
  | ShowContext
  | SideMenuLog M.Focus
  -- * Modifying general node's attributes
  | SetNodeAttr C.NodeId M.Focus Anno.NodeAttr
  -- * Event modification events...
  | SetEventAttr C.NodeId M.Focus Anno.EventAttr
--   | SetEventClass C.NodeId M.Focus Anno.EventClass
--   | SetEventType C.NodeId M.Focus Anno.EventType
--   | SetEventTime C.NodeId M.Focus (Maybe Anno.EventTime)
--   | SetEventAspect C.NodeId M.Focus (Maybe Anno.EventAspect)
  | SetSignalAttr C.NodeId M.Focus Anno.SignalAttr
  | SetTimexAttr C.NodeId M.Focus Anno.TimexAttr
  | CommandStart
  | CommandEnter
  | CommandEscape
  | CommandBackspace
  | CommandComplete
  | CommandChar Char
  | CommandString String
  | Quit
  | Popup              -- ^ Open a popup window
      Popup.Popup
      (Maybe String)   -- ^ The (optionl) HTML ID to focus on
  | QuitPopup
  | SplitBegin
  | SplitChange Int
  | SplitFinish Int
  | ChangeAnnoLevel
  | ChangeAnnoLevelTo M.AnnoLevel
  | SwapFile
  | SwapFileTo C.FileId
  | Compare
  | Dummy
  -- -- | Goto C.Addr -- ^ Move to a given node in the focused window
  | Many (List Msg)
--     -- ^ Tests
--   | TestInput String
--   | TestGet String
--   | TestSend

