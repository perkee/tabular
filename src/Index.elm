module Index exposing
    ( Index, Count
    , Row_, Column_, HLine_, VLine_
    , RowIndex, ColumnIndex, HLineIndex, VLineIndex
    , RowCount, ColumnCount
    , IndexDict, IndexDict2
    , index, toInt, count, countToInt
    , next, prev, add, compareIndex, indexToString
    , countMinusOne, countPlusOne, countIsGreaterThan
    , hLineAbove, hLineBelow, vLineLeft, vLineRight
    , lastHLine, lastVLine
    , rangeCount, rangeFrom, hLineRange, vLineRange, innerVLineRange
    , empty, singleton, insert, get, remove, member, isEmpty, size
    , toList, fromList, keys, values, map, filter, foldl, foldr
    , empty2, singleton2, insert2, get2, remove2, member2, size2
    , toList2, fromList2, map2, filter2, foldl2
    )

import Dict exposing (Dict)


type Index usage
    = Index Int


type Count usage
    = Count Int


type Row_
    = Row_ Never


type Column_
    = Column_ Never


type HLine_
    = HLine_ Never


type VLine_
    = VLine_ Never


type alias RowIndex =
    Index Row_


type alias ColumnIndex =
    Index Column_


type alias HLineIndex =
    Index HLine_


type alias VLineIndex =
    Index VLine_


type alias RowCount =
    Count Row_


type alias ColumnCount =
    Count Column_



-- IndexDict


type IndexDict k v
    = IndexDict (Dict Int v)


type IndexDict2 k1 k2 v
    = IndexDict2 (Dict ( Int, Int ) v)



-- Constructors and extractors


index : Int -> Index a
index =
    Index


toInt : Index a -> Int
toInt (Index i) =
    i


count : Int -> Count a
count =
    Count


countToInt : Count a -> Int
countToInt (Count n) =
    n



-- Arithmetic on Index


next : Index a -> Index a
next (Index i) =
    Index (i + 1)


prev : Index a -> Index a
prev (Index i) =
    Index (i - 1)


add : Int -> Index a -> Index a
add n (Index i) =
    Index (i + n)


compareIndex : Index a -> Index a -> Order
compareIndex (Index a) (Index b) =
    compare a b


indexToString : Index a -> String
indexToString (Index i) =
    String.fromInt i



-- Arithmetic on Count


countMinusOne : Count a -> Count a
countMinusOne (Count n) =
    Count (n - 1)


countPlusOne : Count a -> Count a
countPlusOne (Count n) =
    Count (n + 1)


countIsGreaterThan : Int -> Count a -> Bool
countIsGreaterThan n (Count c) =
    c > n



-- Cross-type conversions


hLineAbove : RowIndex -> HLineIndex
hLineAbove (Index i) =
    Index i


hLineBelow : RowIndex -> HLineIndex
hLineBelow (Index i) =
    Index (i + 1)


vLineLeft : ColumnIndex -> VLineIndex
vLineLeft (Index c) =
    Index c


vLineRight : ColumnIndex -> VLineIndex
vLineRight (Index c) =
    Index (c + 1)


lastHLine : RowCount -> HLineIndex
lastHLine (Count n) =
    Index n


lastVLine : ColumnCount -> VLineIndex
lastVLine (Count n) =
    Index n



-- Range helpers


rangeCount : Count a -> List (Index a)
rangeCount (Count n) =
    List.map Index (List.range 0 (n - 1))


rangeFrom : Index a -> Count a -> List (Index a)
rangeFrom (Index start) (Count n) =
    List.map Index (List.range start (start + n - 1))


hLineRange : RowCount -> List HLineIndex
hLineRange (Count n) =
    List.map Index (List.range 0 n)


vLineRange : ColumnCount -> List VLineIndex
vLineRange (Count n) =
    List.map Index (List.range 0 n)


innerVLineRange : ColumnCount -> List VLineIndex
innerVLineRange (Count n) =
    List.map Index (List.range 1 (n - 1))



-- IndexDict operations


empty : IndexDict k v
empty =
    IndexDict Dict.empty


singleton : Index k -> v -> IndexDict k v
singleton (Index k) v =
    IndexDict (Dict.singleton k v)


insert : Index k -> v -> IndexDict k v -> IndexDict k v
insert (Index k) v (IndexDict d) =
    IndexDict (Dict.insert k v d)


get : Index k -> IndexDict k v -> Maybe v
get (Index k) (IndexDict d) =
    Dict.get k d


remove : Index k -> IndexDict k v -> IndexDict k v
remove (Index k) (IndexDict d) =
    IndexDict (Dict.remove k d)


member : Index k -> IndexDict k v -> Bool
member (Index k) (IndexDict d) =
    Dict.member k d


isEmpty : IndexDict k v -> Bool
isEmpty (IndexDict d) =
    Dict.isEmpty d


size : IndexDict k v -> Int
size (IndexDict d) =
    Dict.size d


toList : IndexDict k v -> List ( Index k, v )
toList (IndexDict d) =
    Dict.toList d |> List.map (\( k, v ) -> ( Index k, v ))


fromList : List ( Index k, v ) -> IndexDict k v
fromList items =
    IndexDict (Dict.fromList (List.map (\( Index k, v ) -> ( k, v )) items))


keys : IndexDict k v -> List (Index k)
keys (IndexDict d) =
    Dict.keys d |> List.map Index


values : IndexDict k v -> List v
values (IndexDict d) =
    Dict.values d


map : (Index k -> a -> b) -> IndexDict k a -> IndexDict k b
map f (IndexDict d) =
    IndexDict (Dict.map (\k v -> f (Index k) v) d)


filter : (Index k -> v -> Bool) -> IndexDict k v -> IndexDict k v
filter f (IndexDict d) =
    IndexDict (Dict.filter (\k v -> f (Index k) v) d)


foldl : (Index k -> v -> acc -> acc) -> acc -> IndexDict k v -> acc
foldl f acc (IndexDict d) =
    Dict.foldl (\k v a -> f (Index k) v a) acc d


foldr : (Index k -> v -> acc -> acc) -> acc -> IndexDict k v -> acc
foldr f acc (IndexDict d) =
    Dict.foldr (\k v a -> f (Index k) v a) acc d



-- IndexDict2 operations


empty2 : IndexDict2 k1 k2 v
empty2 =
    IndexDict2 Dict.empty


singleton2 : Index k1 -> Index k2 -> v -> IndexDict2 k1 k2 v
singleton2 (Index k1) (Index k2) v =
    IndexDict2 (Dict.singleton ( k1, k2 ) v)


insert2 : Index k1 -> Index k2 -> v -> IndexDict2 k1 k2 v -> IndexDict2 k1 k2 v
insert2 (Index k1) (Index k2) v (IndexDict2 d) =
    IndexDict2 (Dict.insert ( k1, k2 ) v d)


get2 : Index k1 -> Index k2 -> IndexDict2 k1 k2 v -> Maybe v
get2 (Index k1) (Index k2) (IndexDict2 d) =
    Dict.get ( k1, k2 ) d


remove2 : Index k1 -> Index k2 -> IndexDict2 k1 k2 v -> IndexDict2 k1 k2 v
remove2 (Index k1) (Index k2) (IndexDict2 d) =
    IndexDict2 (Dict.remove ( k1, k2 ) d)


member2 : Index k1 -> Index k2 -> IndexDict2 k1 k2 v -> Bool
member2 (Index k1) (Index k2) (IndexDict2 d) =
    Dict.member ( k1, k2 ) d


size2 : IndexDict2 k1 k2 v -> Int
size2 (IndexDict2 d) =
    Dict.size d


toList2 : IndexDict2 k1 k2 v -> List ( Index k1, Index k2, v )
toList2 (IndexDict2 d) =
    Dict.toList d |> List.map (\( ( k1, k2 ), v ) -> ( Index k1, Index k2, v ))


fromList2 : List ( Index k1, Index k2, v ) -> IndexDict2 k1 k2 v
fromList2 items =
    IndexDict2 (Dict.fromList (List.map (\( Index k1, Index k2, v ) -> ( ( k1, k2 ), v )) items))


map2 : (Index k1 -> Index k2 -> a -> b) -> IndexDict2 k1 k2 a -> IndexDict2 k1 k2 b
map2 f (IndexDict2 d) =
    IndexDict2 (Dict.map (\( k1, k2 ) v -> f (Index k1) (Index k2) v) d)


filter2 : (Index k1 -> Index k2 -> v -> Bool) -> IndexDict2 k1 k2 v -> IndexDict2 k1 k2 v
filter2 f (IndexDict2 d) =
    IndexDict2 (Dict.filter (\( k1, k2 ) v -> f (Index k1) (Index k2) v) d)


foldl2 : (Index k1 -> Index k2 -> v -> acc -> acc) -> acc -> IndexDict2 k1 k2 v -> acc
foldl2 f acc (IndexDict2 d) =
    Dict.foldl (\( k1, k2 ) v a -> f (Index k1) (Index k2) v a) acc d
