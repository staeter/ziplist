module ZipList exposing
    ( ZipList(..)
    , new, fromList, singleton
    , current, toList, length, currentIndex, isCurrent, isFirst, isLast
    , remove, replace, insert, insertAfter, insertBefore, filter, reverse
    , forward, backward, jumpForward, jumpBackward, maybeJumpForward, maybeJumpBackward
    , goToStart, goToEnd, goToIndex, goToFirst, goToNext, goToLast, goToPrevious
    , map, indexedMap, selectedMap, indexedSelectedMap
    )


{-| A `ZipList` is a list that has a single selected element. We call it current as "the one that is currently selected".

To get more explicit examples, I'm gona represent `ZipList`s as `List`s that have the selected element between "<...>":

    Zipper [] 0 [1, 2, 3, 4]  == [<0>, 1, 2, 3, 4]
    Zipper [2, 1, 0] 3 [4]    == [0, 1, 2, <3>, 4]

This **pseudocode** will make the documentation way more enjoyable.

# ZipLists
@docs ZipList

# Create
@docs new, fromList, singleton

# Consult
@docs current, toList, length, currentIndex, isCurrent, isFirst, isLast

# Edit
@docs remove, replace, insert, insertAfter, insertBefore, filter, reverse

# Move
@docs forward, backward, jumpForward, jumpBackward, maybeJumpForward, maybeJumpBackward

# GoTo's
@docs goToStart, goToEnd, goToIndex, goToFirst, goToNext, goToLast, goToPrevious

# Mapping
@docs map, indexedMap, selectedMap, indexedSelectedMap

-}

import Maybe exposing (Maybe, map, withDefault)
import List.Extra


{-| A `ZipList` is a list that has a single selected element. We call it current as "the one that is currently selected".

    type ZipList a
      = Zipper (List a) a (List a)
-}
type ZipList a
  = Zipper (List a) a (List a)


{-| Craft a new `ZipList` out of a `List` and an element.

    new "a" ["b", "c"] == [<"a">, "b", "c"]
-}
new : a -> List a -> ZipList a
new elem list =
  Zipper [] elem list


{-| Craft a new `ZipList` out of a `List`. Current is the first element of the `List`. Return `Nothing` if an empty list is given.
-}
fromList : List a -> Maybe (ZipList a)
fromList list =
  case list of
    [] -> Nothing
    head :: queue ->
      Just (Zipper [] head queue)


{-| Create a new `ZipList` with a single element in it.

    current (singleton "my element")  == "my element"
    length (singleton "my element")   == 1
-}
singleton : a -> ZipList a
singleton item =
  Zipper [] item []


{-| Return the current element of a `ZipList`.
-}
current : ZipList a -> a
current (Zipper _ elem _) = elem


{-| Convert a `ZipList` into a `List`.

    toList ["a", <"b">, "c"] == ["a", "b", "c"]
-}
toList : ZipList a -> List a
toList (Zipper before elem after) =
  List.concat
    [ List.reverse before
    , List.singleton elem
    , after
    ]


{-| Return a `ZipList` length.

    length [0, 1, <2>, 3, 4]  == 5
    length [<0>, 1, 2]        == 3
-}
length : ZipList a -> Int
length (Zipper before _ after) =
  1 + List.length before + List.length after


{-| Return the index (starting at zero) of the current element.

    currentIndex [0, 1, <2>, 3, 4]  == Just 2
    currentIndex [<0>, 1, 2]        == Just 0
-}
currentIndex : ZipList a -> Int
currentIndex (Zipper before _ _) =
  List.length before


{-| Test wether current passes a condition.

    isCurren odd [0, <1>, 2]  == True
    isCurren odd [0, <5>, 2]  == False
-}
isCurrent : (a -> Bool) -> ZipList a -> Bool
isCurrent condition (Zipper _ elem _) =
  condition elem


{-| Test wether the first element of the `ZipList` passes a condition.

    isFirst odd [1, <1>, 2]  == True
    isFirst odd [4, <5>, 2]  == False
-}
isFirst : (a -> Bool) -> ZipList a -> Bool
isFirst condition zipList =
  case zipList of
    Zipper [] elem _ -> condition elem
    Zipper before _ _ ->
      before
      |> List.Extra.last
      |> Maybe.map condition
      |> Maybe.withDefault False


{-| Test wether the last element of the `ZipList` passes a condition.

    isLast odd [1, <1>, 3]  == True
    isLast odd [4, <5>, 2]  == False
-}
isLast : (a -> Bool) -> ZipList a -> Bool
isLast condition zipList =
  case zipList of
    Zipper _ elem [] -> condition elem
    Zipper _ _ after ->
      after
      |> List.Extra.last
      |> Maybe.map condition
      |> Maybe.withDefault False


{-| Remove current from a `ZipList`. The new current is in priority the `ZipList`'s next element.

    remove [0, 1, <2>, 3, 4] == Just [0, 1, <3>, 4]
    remove [0, 1, <2>]       == Just [0, <1>]
    remove [<"hello!">]      == Nothing
-}
remove : ZipList a -> Maybe (ZipList a)
remove zipList =
  case zipList of
    Zipper [] _ [] -> Nothing
    Zipper before _ (head :: queue) ->
        Zipper before head queue |> Just
    Zipper (head :: queue) _ [] ->
        Zipper queue head [] |> Just


{-| Replace current from a `ZipList` with a new value.

    replace 9 [0, 1, <2>, 3, 4] == [0, 1, <9>, 3, 4]
    replace 9 [0, 1, <2>]       == [0, 1, <9>]
-}
replace : a -> ZipList a -> ZipList a
replace newElem (Zipper before _ after) =
  Zipper before newElem after


{-| Insert a new value in a `ZipList`. The current will be pushed backward to let the new value take its place.

    insert 9 [0, 1, <2>, 3, 4] == [0, 1, 2, <9>, 3, 4]
    insert 9 [0, 1, <2>]       == [0, 1, 2, <9>]
-}
insert : a -> ZipList a -> ZipList a
insert newElem (Zipper before elem after) =
  Zipper (elem :: before) newElem after


{-| Insert a new value in a `ZipList` right after current.

    insertAfter 9 [0, 1, <2>, 3, 4] == [0, 1, <2>, 9, 3, 4]
    insertAfter 9 [0, 1, <2>]       == [0, 1, <2>, 9]
-}
insertAfter : a -> ZipList a -> ZipList a
insertAfter newElem (Zipper before elem after) =
  Zipper before elem (newElem :: after)


{-| Insert a new value in a `ZipList` right before current.

    insertBefore 9 [0, 1, <2>, 3, 4] == [0, 1, 9, <2>, 3, 4]
-}
insertBefore : a -> ZipList a -> ZipList a
insertBefore newElem (Zipper before elem after) =
  Zipper (newElem :: before) elem after


{-| Keep elements that satisfy the test.

    filter ((!=) 3) [0, 3, 3, 1, 3, <2>, 3, 4] == Just [0, 1, <2>, 4]
    filter ((!=) 2) [0, 1, <2>, 3, 4] == Just [0, 1, <3>, 4]
    filter ((!=) 4) [0, 1, 2, 3, <4>] == Just [0, 1, 2, <3>]
    filter ((!=) 4) [4, <4>, 4, 4] == Nothing
-}
filter : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
filter condition (Zipper before elem after) =
  let
    filteredBefore =
      List.filter condition before

    filteredAfter =
      List.filter condition after

    filteredCurrent =
      if condition elem
      then Just elem
      else Nothing
  in
  filteredCurrent
  |> Maybe.map (\ c -> Zipper filteredBefore c filteredAfter |> Just )
  |> Maybe.withDefault
        ( case (filteredBefore, filteredAfter) of
            ([], []) -> Nothing
            (head :: queue, []) ->
                Zipper queue head [] |> Just
            (beforeBis, head :: queue) ->
                Zipper beforeBis head queue |> Just
        )


{-| Reverse `ZipList` order.

    filter [1, <2>, 3] == [3, <2>, 1]
-}
reverse : ZipList a -> ZipList a
reverse (Zipper before elem after) =
  Zipper after elem before


{-| Move current forward. Current will not move if it is at the end of the `ZipList`.

    forward [0, 1, <2>, 3, 4] == [0, 1, 2, <3>, 4]
    forward [0, 1, <2>]       == [0, 1, <2>]
-}
forward : ZipList a -> ZipList a
forward zipList =
  let (Zipper before elem after) = zipList in
  case after of
    [] -> zipList
    head :: queue ->
      Zipper (elem :: before) head queue


{-| Move current backward. Current will not move if it is at the begining of the `ZipList`.

    backward [0, 1, <2>, 3, 4] == [0, <1>, 2, 3, 4]
    backward [<0>, 1, 2]       == [<0>, 1, 2]
-}
backward : ZipList a -> ZipList a
backward zipList =
  let (Zipper before elem after) = zipList in
  case before of
    [] -> zipList
    head :: queue ->
      Zipper queue head (elem :: after)


{-| Move current forward a given amout of times. Current will be the last element of the `ZipList` if the jump size is too big.

    jumpForward 2 [0, <1>, 2, 3, 4] == [0, 1, 2, <3>, 4]
    jumpForward 2 [0, <1>, 2]       == [0, 1, <2>]
-}
jumpForward : Int -> ZipList a -> ZipList a
jumpForward jumpSize zipList =
  if jumpSize <= 0
  then zipList
  else case zipList of
    Zipper before elem (head :: queue) ->
        Zipper (elem :: before) head queue
        |> jumpForward (jumpSize - 1)
    _ -> zipList


{-| Move current backward a given amout of times. Current will be the first element of the `ZipList` if the jump size is too big.

    jumpBackward 2 [0, 1, 2, <3>, 4] == [0, <1>, 2, 3, 4]
    jumpBackward 2 [0, <1>, 2]       == [<0>, 1, 2]
-}
jumpBackward : Int -> ZipList a -> ZipList a
jumpBackward jumpSize zipList =
  if jumpSize <= 0
  then zipList
  else case zipList of
    Zipper (head :: queue) elem after ->
        Zipper queue head (elem :: after)
        |> jumpBackward (jumpSize - 1)
    _ -> zipList


{-| Move current forward a given amout of times. Return `Nothing` if the jump size is too big.

    maybeJumpForward 2 [0, <1>, 2, 3, 4] == Just [0, 1, 2, <3>, 4]
    maybeJumpForward 2 [0, <1>, 2]       == Nothing
-}
maybeJumpForward : Int -> ZipList a -> Maybe (ZipList a)
maybeJumpForward jumpSize zipList =
  if jumpSize <= 0
  then Just zipList
  else case zipList of
    Zipper before elem [] -> Nothing
    Zipper before elem (head :: queue) ->
        Zipper (elem :: before) head queue
        |> maybeJumpForward (jumpSize - 1)


{-| Move current backward a given amout of times. Return `Nothing` if the jump size is too big.

    maybeJumpBackward 2 [0, 1, 2, <3>, 4] == Just [0, <1>, 2, 3, 4]
    maybeJumpBackward 2 [0, <1>, 2]       == Nothing
-}
maybeJumpBackward : Int -> ZipList a -> Maybe (ZipList a)
maybeJumpBackward jumpSize zipList =
  if jumpSize <= 0
  then Just zipList
  else case zipList of
    Zipper [] elem after -> Nothing
    Zipper (head :: queue) elem after ->
        Zipper queue head (elem :: after)
        |> maybeJumpBackward (jumpSize - 1)


{-| Move current to index 0.

    goToStart [0, 1, 2, 3, <4>] == [<0>, 1, 2, 3, 4]
    goToStart [<0>, 1, 2]       == [<0>, 1, 2]
    goToStart [0, <1>, 2]       == [<0>, 1, 2]
-}
goToStart : ZipList a -> ZipList a
goToStart zipList =
  let (Zipper before _ _) = zipList in
  case before of
    [] -> zipList
    head :: queue ->
      backward zipList
      |> goToStart


{-| Move current to the end of the `ZipList`.

    goToEnd [0, 1, 2, 3, <4>]   == [0, 1, 2, 3, <4>]
    goToEnd [<0>, 1, 2]         == [0, 1, <2>]
    goToEnd [0, <1>, 2]         == [0, 1, <2>]
-}
goToEnd : ZipList a -> ZipList a
goToEnd zipList =
  let (Zipper _ _ after) = zipList in
  case after of
    [] -> zipList
    head :: queue ->
      forward zipList
      |> goToEnd


{-| Move current to an index (starting at zero). Return `Nothing` if the index is too low and it will be the last element if the index is too high.

    goToIndex 2 [0, 1, 2, 3, <4>] == Just [0, 1, <2>, 3, 4]
    goToIndex 5 [0, <1>, 2]       == Nothing
    goToIndex 1 [0, <1>, 2]       == Just [0, <1>, 2]
-}
goToIndex : Int -> ZipList a -> Maybe (ZipList a)
goToIndex newIndex zipList =
  let
    index = currentIndex zipList
    delta = newIndex - index
  in
  case sign delta of
    Zero -> Just zipList
    Positif ->
      maybeJumpForward delta zipList
    Negatif ->
      maybeJumpBackward (abs delta) zipList


{-| Move current to the first element of a `ZipList` fulfilling a condition. Return `Nothing` if no matching element.

    goToFirst isEven [8, 1, 2, 3, <4>] == Just [<8>, 1, 2, 3, 4]
    goToFirst isEven [5, <1>, 2]       == Just [5, 1, <2>]
    goToFirst isEven [1, <1>, 7]       == Nothing
-}
goToFirst : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
goToFirst condition zipList =
  let newZipList = goToStart zipList in
  if isCurrent condition newZipList
  then Just newZipList
  else goToNext condition newZipList



{-| Move current to the next element fulfilling a condition. Return `Nothing` if there is no matching element after current.

    goToNext isEven [0, 1, <2>, 3, 4] == Just [0, 1, 2, 3, <4>]
    goToNext isEven [8, 2, <1>, 3, 7] == Nothing
    goToNext isEven [5, <1>, 2, 3]    == Just [5, 1, <2>, 3]
-}
goToNext : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
goToNext condition zipList =
  let (Zipper _ _ after) = zipList in
  if List.any condition after
  then goToNextSub condition (forward zipList) |> Just
  else Nothing

goToNextSub : (a -> Bool) -> ZipList a -> ZipList a
goToNextSub condition zipList =
  let (Zipper _ _ after) = zipList in
  if List.any condition after
  then goToNextSub condition (forward zipList)
  else zipList


{-| Move current to the last element of a `ZipList` fulfilling a condition. Return `Nothing` if there is no matching element.

    goToLast isOdd [0, 1, <2>, 3, 4] == Just [0, 1, 2, <3>, 4]
    goToLast isOdd [8, 2, <1>, 3, 7] == Just [8, 2, 1, 3, <7>]
    goToLast isOdd [2, <4>, 6, 8]    == Nothing
-}
goToLast : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
goToLast condition zipList =
  let newZipList = goToEnd zipList in
  if isCurrent condition newZipList
  then Just newZipList
  else goToPrevious condition newZipList


{-| Move current to the previous element fulfilling a condition. Return `Nothing` if there is no matching element before current.

    goToPrevious isOdd [0, 1, <2>, 3, 4] == Just [0, <1>, 2, 3, 4]
    goToPrevious isOdd [8, 2, 1, 4, <7>] == Just [8, 2, <1>, 4, 7]
    goToPrevious isOdd [2, <4>, 3, 5]    == Nothing
-}
goToPrevious : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
goToPrevious condition zipList =
  let (Zipper before _ _) = zipList in
  if List.any condition before
  then goToPreviousSub condition (backward zipList) |> Just
  else Nothing

goToPreviousSub : (a -> Bool) -> ZipList a -> ZipList a
goToPreviousSub condition zipList =
  let (Zipper before _ _) = zipList in
  if List.any condition before
  then goToPreviousSub condition (backward zipList)
  else zipList


{-| Apply a function to every element of a `ZipList`.

    map String.fromInt [0, 1, <2>, 3, 4] == ["0", "1", <"2">, "3", "4"]
    map String.fromInt [2, <4>, 3, 5]    == ["2", <"4">, "3", "5"]
-}
map : (a -> b) -> ZipList a -> ZipList b
map func (Zipper before elem after) =
  Zipper
    (List.map func before)
    (func elem)
    (List.map func after)


{-| Same as `map` but the function is also applied to the index of each element (starting at zero).

    indexedMap Tuple.pair [1, <2>, 4]     == [(0, 1), <(1, 2)>, (2, 4)]
    indexedMap Tuple.pair ["hi", <"wow">] == [(0, "hi"), <(1, "wow")>]
-}
indexedMap : (Int -> a -> b) -> ZipList a -> ZipList b
indexedMap func (Zipper before elem after) =
  let index = List.length before in
  Zipper
    (List.indexedMap
      (\ indexBe elemBe -> func (index - 1 - indexBe) elemBe )
        before
    )
    (func index elem)
    (List.indexedMap
      (\ indexAf elemAf -> func (index + 1 + indexAf) elemAf )
      after
    )


{-| Same as `map` but the function also takes a boolean indicating wether it is current/the selected element.

    selectedMap Tuple.pair [<2>, 4]             == [<(True, 2)>, (False, 4)]
    selectedMap Tuple.pair ["en", "fr", <"ge">] == [(False, "en"), (False, "fr"), <(True, "ge")>]
-}
selectedMap : (Bool -> a -> b) -> ZipList a -> ZipList b
selectedMap func (Zipper before elem after) =
  Zipper
    (List.map (func False) before)
    (func True elem)
    (List.map (func False) after)


{-| Same as `map` but the function also takes the index of the element (starting at zero) and a boolean indicating wether it is current/the selected element.

    let
      myFun =
        (\ index isCurrent elem ->
          (index, isCurrent, String.fromInt elem)
        )
    in
      selectedMap myFun [1, <2>, 4] == [(0, False, "1"), <(1, True, "2")>, (2, False, "4")]
-}
indexedSelectedMap : (Int -> Bool -> a -> b) -> ZipList a -> ZipList b
indexedSelectedMap func (Zipper before elem after) =
  let index = List.length before in
  Zipper
    (List.indexedMap
      (\ indexBe elemBe -> func (index - 1 - indexBe) False elemBe )
        before
    )
    (func index True elem)
    (List.indexedMap
      (\ indexAf elemAf -> func (index + 1 + indexAf) False elemAf )
      after
    )


-- not exposed code

type Sign
  = Zero
  | Positif
  | Negatif

sign : Int -> Sign
sign val =
  if val == 0
  then Zero
  else
    if val > 0
    then Positif
    else Negatif
