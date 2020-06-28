module Todo.Importance exposing
    ( Importance(..)
    , compare
    , getDisplayData
    )

import Color exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Importance
    = NoImportance
    | Want
    | Need



-- COMPARISONS


compare : Importance -> Importance -> Order
compare a b =
    Basics.compare
        (asIndex a)
        (asIndex b)


asIndex : Importance -> Int
asIndex imp =
    case imp of
        NoImportance ->
            0

        Want ->
            1

        Need ->
            2



-- DISPLAY


getDisplayData : Importance -> ( Color, String )
getDisplayData imp =
    case imp of
        Need ->
            ( Color.red, "NEED" )

        Want ->
            ( Color.orange, "WANT" )

        NoImportance ->
            ( Color.darkGreen, "NOT IMPORTANT" )
