port module Ports exposing (JsMsg, Listener, listen, send)

import Dict exposing (Dict)
import Json.Decode exposing (Value)



-- RAW PORTS


port elmToJs : ( String, Value ) -> Cmd msg


port jsToElm : (( String, Value ) -> msg) -> Sub msg



-- TYPES


type alias JsMsg =
    ( String, Value )


type alias Listener msg =
    ( String, Value -> msg )



-- INTERFACE


send : JsMsg -> Cmd msg
send =
    elmToJs


listen : (String -> msg) -> List (Listener msg) -> Sub msg
listen onBadMsg listeners =
    let
        callbacks =
            Dict.fromList listeners

        router =
            \( msg, value ) ->
                case Dict.get msg callbacks of
                    Just callback ->
                        callback value

                    Nothing ->
                        onBadMsg msg
    in
    jsToElm router
