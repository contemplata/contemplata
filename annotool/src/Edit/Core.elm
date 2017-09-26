-- | Core types

module Edit.Core exposing
  ( FileId
  , NodeId
  , TreeId
  , Addr
  )


type alias FileId = String


-- | Tree identifier
type alias TreeId = Int
-- type alias TreeId = String


-- | Internal node identifier
type alias NodeId = Int


type alias Addr = (TreeId, NodeId)
