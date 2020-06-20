module Todo.Save exposing (Format(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Todo.Item as Item exposing (Item)


type Format
    = V1 (List Item)


encode : Format -> Value
encode format =
    case format of
        V1 list ->
            Encode.object
                [ ( "version", Encode.int 1 )
                , ( "items", Encode.list Item.encode list )
                ]


decoder : Decoder Format
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen decodeVersion


decodeVersion : Int -> Decoder Format
decodeVersion version =
    case version of
        1 ->
            Decode.map V1 (Decode.field "items" (Decode.list Item.decoder))

        v ->
            Decode.fail ("Unrecognized Save Format Version: " ++ String.fromInt v)
