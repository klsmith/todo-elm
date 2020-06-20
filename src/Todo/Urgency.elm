module Todo.Urgency exposing
    ( Urgency(..)
    , compare
    , decoder
    , encode
    , getDisplayData
    , parse
    )

import Color exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- import Time exposing (Posix)


type Urgency
    = Whenever
    | Eventually
      -- | Deadline Posix
    | Soon
    | Asap


encode : Urgency -> Value
encode urgency =
    case urgency of
        Whenever ->
            Encode.string "Whenever"

        Eventually ->
            Encode.string "Eventually"

        Soon ->
            Encode.string "Soon"

        Asap ->
            Encode.string "Asap"


decoder : Decoder Urgency
decoder =
    Decode.string |> Decode.andThen decoderFromString


decoderFromString : String -> Decoder Urgency
decoderFromString string =
    case string of
        "Whenever" ->
            Decode.succeed Whenever

        "Eventually" ->
            Decode.succeed Eventually

        "Soon" ->
            Decode.succeed Soon

        "Asap" ->
            Decode.succeed Asap

        _ ->
            Decode.fail ("Unrecognized Urgency Value: " ++ string)


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

        -- Deadline _ ->
        --     2
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

        -- Deadline _ ->
        --     ( Color.orange, "DEADLINE ???" )
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
