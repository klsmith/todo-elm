port module Ports exposing (JsMsg, Listeners, asSubscriptions, noopListener, register, send)

import Dict exposing (Dict)
import Json.Decode exposing (Value)



-- HIDDEN PORTS


port send : JsMsg -> Cmd msg


port listen : (JsMsg -> msg) -> Sub msg



-- HIDDEN PORT TYPES


type alias JsMsg =
    ( String, Value )



-- LISTENERS


type Listeners msg
    = Listeners msg (Dict String (Value -> msg))


noopListener : msg -> Listeners msg
noopListener noop =
    Listeners noop Dict.empty


register : String -> (Value -> msg) -> Listeners msg -> Listeners msg
register msg listener (Listeners noop listeners) =
    Listeners noop (listeners |> Dict.insert msg listener)


asSubscriptions : Listeners msg -> Sub msg
asSubscriptions (Listeners noop listeners) =
    listen
        (\( msg, value ) ->
            case Dict.get msg listeners of
                Just listener ->
                    listener value

                Nothing ->
                    noop
        )
