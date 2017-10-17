-- | Core types

module Edit.Core exposing
  ( FileId
  , AnnoName
  , NodeId
  , TreeId
  , Addr
  )


type alias FileId = String


-- | Annotator identifier.
type alias AnnoName = String


-- | Tree identifier
type alias TreeId = Int
-- type alias TreeId = String


-- | Internal node identifier
type alias NodeId = Int


type alias Addr = (TreeId, NodeId)
