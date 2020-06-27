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


listen : msg -> List (Listener msg) -> Sub msg
listen noop listeners =
    let
        callbacks =
            Dict.fromList listeners

        router =
            \( msg, value ) ->
                case Dict.get msg callbacks of
                    Just callback ->
                        callback value

                    Nothing ->
                        noop
    in
    jsToElm router
