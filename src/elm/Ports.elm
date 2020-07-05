port module Ports exposing (JsMsg, Listener, listen, listener, send)

import Dict
import Json.Decode as Decode exposing (Decoder, Value)



-- RAW PORTS


port elmToJs : ( String, Value ) -> Cmd msg


port jsToElm : (( String, Value ) -> msg) -> Sub msg



-- TYPES


type alias JsMsg =
    ( String, Value )


type Listener msg
    = Listener String (Value -> msg)



-- INTERFACE


send : JsMsg -> Cmd msg
send =
    elmToJs


listener :
    { key : String
    , decoder : Decoder a
    , callback : Result Decode.Error a -> msg
    }
    -> Listener msg
listener { key, decoder, callback } =
    Listener key (Decode.decodeValue decoder >> callback)


listen : (String -> msg) -> List (Listener msg) -> Sub msg
listen onBadMsg listeners =
    let
        callbacks =
            listeners
                |> List.map destructListener
                |> Dict.fromList

        router =
            \( msg, value ) ->
                case Dict.get msg callbacks of
                    Just callback ->
                        callback value

                    Nothing ->
                        onBadMsg msg
    in
    jsToElm router


destructListener : Listener msg -> ( String, Value -> msg )
destructListener (Listener k c) =
    ( k, c )
