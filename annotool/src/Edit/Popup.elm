module Edit.Popup exposing
  ( Popup(..)
  , SplitPopup
  )


import Edit.Core as C


-- | Node in a syntactic tree is either an internal node or a leaf.
type Popup
  = Files (Maybe (List C.FileId)) -- Quit popup
  | Info String -- Generic information popup
  | Split SplitPopup


type alias SplitPopup =
  { word : String
  , split : Int
  }
