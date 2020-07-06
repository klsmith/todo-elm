module Todo.Save exposing (Format, decoder, deformat, encode, format)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time
import Todo.Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Urgency exposing (Urgency(..))


type Format
    = V1 (List Item)



-- FORMATTING


format : List Item -> Format
format items =
    V1 items


deformat : Format -> List Item
deformat (V1 items) =
    items



-- ENCODING


encode : Format -> Value
encode save =
    case save of
        V1 list ->
            Encode.object
                [ ( "version", Encode.int 1 )
                , ( "items", Encode.list itemEncoder list )
                ]


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "importance", importanceEncoder (Item.getImportance item) )
        , ( "urgency", urgencyEncoder (Item.getUrgency item) )
        , ( "details", Encode.string (Item.getDetails item) )
        , ( "rawText", Encode.string (Item.getRawText item) )
        ]


importanceEncoder : Importance -> Value
importanceEncoder importance =
    case importance of
        NoImportance ->
            Encode.string "NoImportance"

        Want ->
            Encode.string "Want"

        Need ->
            Encode.string "Need"


urgencyEncoder : Urgency -> Value
urgencyEncoder urgency =
    case urgency of
        Whenever ->
            Encode.string "Whenever"

        Eventually ->
            Encode.string "Eventually"

        Event posix ->
            Encode.int <| Time.posixToMillis posix

        Soon ->
            Encode.string "Soon"

        Asap ->
            Encode.string "Asap"



-- DECODING


decoder : Decoder Format
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen decodeVersion


decodeVersion : Int -> Decoder Format
decodeVersion version =
    case version of
        1 ->
            Decode.map V1 (Decode.field "items" (Decode.list itemDecoder))

        v ->
            Decode.fail ("Unrecognized Save Format Version: " ++ String.fromInt v)


itemDecoder : Decoder Item
itemDecoder =
    Decode.map4 Item.create
        (Decode.field "importance" importanceDecoder)
        (Decode.field "urgency" urgencyDecoder)
        (Decode.field "details" Decode.string)
        (Decode.field "rawText" Decode.string)


importanceDecoder : Decoder Importance
importanceDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "NoImportance" ->
                        Decode.succeed NoImportance

                    "Want" ->
                        Decode.succeed Want

                    "Need" ->
                        Decode.succeed Need

                    _ ->
                        Decode.fail ("Unrecognized Importance Value: " ++ string)
            )


urgencyDecoder : Decoder Urgency
urgencyDecoder =
    Decode.oneOf
        [ Decode.map Event (Decode.map Time.millisToPosix Decode.int)
        , Decode.string
            |> Decode.andThen
                (\string ->
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
                )
        ]
