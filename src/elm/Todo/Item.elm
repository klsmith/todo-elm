module Todo.Item exposing
    ( Item
    , create
    , equals
    , getDetails
    , getImportance
    , getRawText
    , getUrgency
    )

import Todo.Importance as Importance exposing (Importance(..))
import Todo.Urgency as Urgency exposing (Urgency(..))



-- TYPES


type Item
    = Item Importance Urgency String String



-- CONSTRUCTORS


create : Importance -> Urgency -> String -> String -> Item
create =
    Item



-- COMPARISONS


equals : Item -> Item -> Bool
equals =
    (==)



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
