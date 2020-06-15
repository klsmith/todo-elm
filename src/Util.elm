module Util exposing
    ( andThenCompareWith
    , applyTuple
    , maybePrepend
    , tern
    )


andThenCompareWith : (a -> a -> Order) -> a -> a -> Order -> Order
andThenCompareWith comparator a b higherOrder =
    if higherOrder == EQ then
        comparator a b

    else
        higherOrder


applyTuple : (a -> b -> c) -> ( a, b ) -> c
applyTuple funct ( a, b ) =
    funct a b


{-| Similar to the ternary operator (?:) from other languages (bool ? ifTrue : ifFalse).

    This function takes a Tuple of two values of the same type and a Bool. If True, it returns the first value from the Tuple, otherwise it returns the second value from the Tuple.

    Typical Usage:
        myBool |> tern (choiceA, choiceB)

-}
tern : ( a, a ) -> Bool -> a
tern ( first, second ) bool =
    if bool then
        first

    else
        second


maybePrepend : Maybe a -> List a -> List a
maybePrepend maybe list =
    case maybe of
        Just value ->
            value :: list

        Nothing ->
            list
