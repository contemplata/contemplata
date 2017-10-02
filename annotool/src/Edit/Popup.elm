module Edit.Popup exposing
  ( Popup(..)
  , SplitPopup
  )


-- | Node in a syntactic tree is either an internal node or a leaf.
type Popup
  = Files -- SavePopup
  | Info String -- Generic information popup
  | Split SplitPopup


type alias SplitPopup =
  { word : String
  , split : Int
  }
