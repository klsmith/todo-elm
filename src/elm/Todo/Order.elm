module Todo.Order exposing
    ( byImportanceAscending
    , byImportanceDescending
    , byItemAscending
    , byUrgencyAscending
    , byUrgencyDescending
    )

import Todo.Importance as Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Urgency as Urgency exposing (Urgency(..))


byItemDescending : Item -> Item -> Order
byItemDescending a b =
    flip <| byItemAscending a b


byItemAscending : Item -> Item -> Order
byItemAscending a b =
    byImportanceAscending
        (Item.getImportance a)
        (Item.getImportance b)
        |> tieBreaker byUrgencyAscending
            (Item.getUrgency a)
            (Item.getUrgency b)
        |> tieBreaker compare
            (Item.getDetails a)
            (Item.getDetails b)


tieBreaker : (a -> a -> Order) -> a -> a -> Order -> Order
tieBreaker comparator a b higherOrder =
    if higherOrder == EQ then
        comparator a b

    else
        higherOrder


flip : Order -> Order
flip order =
    case order of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


byImportanceDescending : Importance -> Importance -> Order
byImportanceDescending a b =
    flip <| byImportanceAscending a b


byImportanceAscending : Importance -> Importance -> Order
byImportanceAscending a b =
    let
        asIndex : Importance -> Int
        asIndex imp =
            case imp of
                NoImportance ->
                    0

                Want ->
                    1

                Need ->
                    2
    in
    Basics.compare
        (asIndex a)
        (asIndex b)


byUrgencyDescending : Urgency -> Urgency -> Order
byUrgencyDescending a b =
    flip <| byUrgencyAscending a b


byUrgencyAscending : Urgency -> Urgency -> Order
byUrgencyAscending a b =
    let
        asIndex : Urgency -> Int
        asIndex urg =
            case urg of
                Whenever ->
                    0

                Eventually ->
                    1

                Event _ ->
                    3

                Soon ->
                    4

                Asap ->
                    5
    in
    Basics.compare
        (asIndex a)
        (asIndex b)
