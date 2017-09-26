module Edit.Popup exposing
  ( Popup(..)
  )


-- | Node in a syntactic tree is either an internal node or a leaf.
type Popup
  = Files -- SavePopup
  | Info String -- Generic information popup


-- type alias SavePopup =
--   { }