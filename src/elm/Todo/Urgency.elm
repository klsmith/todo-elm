module Todo.Urgency exposing
    ( Urgency(..)
    , toDisplayString
    )

import Time exposing (Posix)


type Urgency
    = Whenever
    | Eventually
    | Event Posix
    | Soon
    | Asap


toDisplayString : Urgency -> String
toDisplayString urg =
    case urg of
        Whenever ->
            "WHENEVER"

        Eventually ->
            "EVENTUALLY"

        Event posix ->
            "EVENT " ++ (String.fromInt <| Time.posixToMillis <| posix)

        Soon ->
            "SOON"

        Asap ->
            "ASAP"
