module Util exposing (andThenCompareWith)


andThenCompareWith : (a -> a -> Order) -> a -> a -> Order -> Order
andThenCompareWith comparator a b higherOrder =
    if higherOrder == EQ then
        comparator a b

    else
        higherOrder
