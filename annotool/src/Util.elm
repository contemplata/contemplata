module Util exposing
    ( split, splitAt, catMaybes, find, at, unless, mappend, mconcat, guard, check, and
    , intercalate, mapAccumL, average, single, px, isJust, unions
    -- * JSON
    , encodeMaybe
    -- * Dict
    , fromListWith
    -- * HTML
    , (:>)
    )


import List as L
import Set as S
import Dict as D
import Json.Encode as Encode


-- | The results is (ls, rs) where `ls` are all the left-most elements which do
-- | not satisfy the given predicate, while `rs` is the rest. In particular, the
-- | first element of `rs` satisfies the predicate.
split : (a -> Bool) -> List a -> (List a, List a)
split p xs =
  let
    ys = case xs of
      [] -> ([], [])
      hd :: tl ->
        if p hd
          then ([], xs)
          else
            let (ls, rs) = split p tl
            in  (hd :: ls, rs)
     -- revFst (x, y) = (List.reverse x, y)
  in
    -- revFst ys
    ys


-- | Split the list on the given position k.  Equivalent to (take k xs, drop k xs).
splitAt : Int -> List a -> (List a, List a)
splitAt k xs = (L.take k xs, L.drop k xs)


catMaybes : List (Maybe a) -> List a
catMaybes xs =
  case xs of
    [] -> []
    hd :: tl ->
      case hd of
        Nothing -> catMaybes tl
        Just x  -> x :: catMaybes tl


-- | Find the first element which satisfies a given predicate.
find : (a -> Bool) -> List a -> Maybe a
find p xs =
  case xs of
    [] -> Nothing
    hd :: tl ->
      case p hd of
        True  -> Just hd
        False -> find p tl


-- | Retrieve the element on the given position.
at : Int -> List a -> Maybe a
at i xs =
  case (i, xs) of
    (_, []) -> Nothing
    (0, hd :: _) -> Just hd
    (_, _ :: tl) -> at (i - 1) tl


mapAccumL : (acc -> a -> (acc, b)) -> acc -> List a -> (acc, List b)
mapAccumL f acc xs =
  case xs of
    [] -> (acc, [])
    x :: tl ->
      let
        (acc1, y) = f acc x
        (acc2, ys) = mapAccumL f acc1 tl
      in
        (acc2, y :: ys)


intercalate : a -> List a -> List a
intercalate p xs =
    case xs of
        x :: y :: rest -> x :: p :: intercalate p (y :: rest)
        _ -> xs


-- unions : List (S.Set comparable) -> S.Set comparable
-- unions xs =
--   case xs of
--     [] -> S.empty
--     x :: tl -> S.union x (unions tl)


unions : List (S.Set comparable) -> S.Set comparable
unions xs =
    case xs of
        head :: tail -> S.union head (unions tail)
        [] -> S.empty


unless : Bool -> a -> Maybe a
unless flag x = case flag of
  True -> Nothing
  False -> Just x


mappend : Maybe a -> Maybe a -> Maybe a
mappend x y = case (x, y) of
  (Just v, _)  -> Just v
  (Nothing, v) -> v


mconcat : List (Maybe a) -> Maybe a
mconcat xs = case xs of
  hd :: tl -> mappend hd (mconcat tl)
  [] -> Nothing


guard : Bool -> Maybe ()
guard x = case x of
  False -> Nothing
  True  -> Just ()


check : (a -> Bool) -> a -> Maybe a
check p x = case p x of
  False -> Nothing
  True  -> Just x


isJust : Maybe a -> Bool
isJust x = case x of
  Nothing -> False
  _  -> True


and : List Bool -> Bool
and xs = case xs of
  [] -> True
  hd :: tl -> hd && and tl


average : List Float -> Float
average xs = L.sum xs / toFloat (L.length xs)


single : a -> List a
single x = [x]


px : Int -> String
px number =
  toString number ++ "px"


---------------------------------------------------
-- JSON
---------------------------------------------------


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe enc may =
    case may of
        Nothing -> Encode.null
        Just x -> enc x


---------------------------------------------------
-- Dict
---------------------------------------------------


fromListWith
     : (a -> a -> a)
    -> List (comparable, a)
    -> D.Dict comparable a
fromListWith f xs =
    let
        update newVal mayOldVal =
            case mayOldVal of
                Nothing -> Just <| newVal
                Just oldVal -> Just <| f oldVal newVal
    in
        case xs of
            [] -> D.empty
            (key, val) :: tl ->
                D.update key (update val) (fromListWith f tl)


---------------------------------------------------
-- HTML style
---------------------------------------------------


-- | Useful for defining HTML styles.
(:>) : a -> b -> (a, b)
(:>) = (,)
