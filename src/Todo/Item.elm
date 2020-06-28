module Todo.Item exposing
    ( Item
    , compare
    , create
    , equals
    , getDetails
    , getImportance
    , getRawText
    , getUrgency
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Urgency as Urgency exposing (Urgency(..))
import Util exposing (andThenCompareWith)



-- TYPES


type Item
    = Item Importance Urgency String String



-- CONSTRUCTORS


create : Importance -> Urgency -> String -> String -> Item
create =
    Item



-- COMPARISONS


equals : Item -> Item -> Bool
equals itemA itemB =
    compare itemA itemB == EQ


compare : Item -> Item -> Order
compare itemA itemB =
    Importance.compare
        (getImportance itemA)
        (getImportance itemB)
        |> andThenCompareWith
            Urgency.compare
            (getUrgency itemA)
            (getUrgency itemB)
        |> andThenCompareWith
            Basics.compare
            (getDetails itemA)
            (getDetails itemB)



-- GETTERS


getImportance : Item -> Importance
getImportance (Item imp _ _ _) =
    imp


getUrgency : Item -> Urgency
getUrgency (Item _ urg _ _) =
    urg


getDetails : Item -> String
getDetails (Item _ _ details _) =
    details


getRawText : Item -> String
getRawText (Item _ _ _ rawText) =
    rawText
