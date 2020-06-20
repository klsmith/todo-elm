module Todo.Importance exposing
    ( Importance(..)
    , compare
    , decoder
    , encode
    , getDisplayData
    , parse
    )

import Color exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Importance
    = NoImportance
    | Want
    | Need


encode : Importance -> Value
encode importance =
    case importance of
        NoImportance ->
            Encode.string "NoImportance"

        Want ->
            Encode.string "Want"

        Need ->
            Encode.string "Need"


decoder : Decoder Importance
decoder =
    Decode.string |> Decode.andThen decoderFromString


decoderFromString : String -> Decoder Importance
decoderFromString string =
    case string of
        "NoImportance" ->
            Decode.succeed NoImportance

        "Want" ->
            Decode.succeed Want

        "Need" ->
            Decode.succeed Need

        _ ->
            Decode.fail ("Unrecognized Importance Value: " ++ string)


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


getDisplayData : Importance -> ( Color, String )
getDisplayData imp =
    case imp of
        Need ->
            ( Color.red, "NEED" )

        Want ->
            ( Color.orange, "WANT" )

        NoImportance ->
            ( Color.darkGreen, "NOT IMPORTANT" )


parse : String -> Maybe Importance
parse string =
    case
        string
            |> String.filter Char.isAlphaNum
            |> String.toUpper
    of
        "NEED" ->
            Just Need

        "WANT" ->
            Just Want

        _ ->
            Nothing
