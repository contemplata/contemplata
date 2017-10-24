-- | Core types

module Edit.Core exposing
  ( FileId
  , AnnoName
  , NodeId
  , Addr

  , TreeId (..)
  , TreeIdBare
  , unTreeId

  -- * PartId
  , PartId
--   , unPartId
--   , cmpPartId
  )


type alias FileId = String


-- | Annotator identifier.
type alias AnnoName = String


-- | Tree identifier
type TreeId = TreeId TreeIdBare


-- | Tree identifier, bare version.  Useful because `TreeId` has no Ord instance...
type alias TreeIdBare = Int


unTreeId : TreeId -> TreeIdBare
unTreeId (TreeId treeId) = treeId


-- | Internal node identifier
type alias NodeId = Int


type alias Addr = (PartId, NodeId)


---------------------------------------------------
-- Partition IDs
---------------------------------------------------


-- | Partition identifier
type alias PartId = Int


-- cmpPartId : PartId -> PartId -> (PartId, PartId)
-- cmpPartId (PartId tid1) (PartId tid2) =
--     let
--         newRepr = if tid1 < tid2 then tid1 else tid2
--         oldRepr = if tid1 < tid2 then tid2 else tid1
--     in
--         (PartId newRepr, PartId oldRepr)
