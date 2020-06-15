module Todo.Urgency exposing
    ( Urgency(..)
    , compare
    , getDisplayData
    , parse
    )

import Color exposing (Color)
import Time exposing (Posix)


type Urgency
    = Whenever
    | Eventually
    | Deadline Posix
    | Soon
    | Asap


compare : Urgency -> Urgency -> Order
compare a b =
    Basics.compare
        (asIndex a)
        (asIndex b)


asIndex : Urgency -> Int
asIndex urg =
    case urg of
        Whenever ->
            0

        Eventually ->
            1

        Deadline _ ->
            2

        Soon ->
            3

        Asap ->
            4


getDisplayData : Urgency -> ( Color, String )
getDisplayData urg =
    case urg of
        Whenever ->
            ( Color.darkGreen, "WHENEVER" )

        Eventually ->
            ( Color.darkYellow, "EVENTUALLY" )

        Deadline _ ->
            ( Color.orange, "DEADLINE ???" )

        Soon ->
            ( Color.orange, "SOON" )

        Asap ->
            ( Color.red, "ASAP" )


parse : String -> Maybe Urgency
parse string =
    case
        string
            |> String.filter Char.isAlphaNum
            |> String.toUpper
    of
        "ASAP" ->
            Just Asap

        "SOON" ->
            Just Soon

        "EVENTUALLY" ->
            Just Eventually

        "WHENEVER" ->
            Just Whenever

        _ ->
            Nothing
