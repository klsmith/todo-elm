module Ports.LocalStorage exposing
    ( Config
    , Error(..)
    , StorageResult(..)
    , config
    , onLoad
    , requestLoad
    , save
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports



-- TYPES


type Config a
    = Config Key (Encoder a) (Decoder a)


type StorageResult a
    = NotStored
    | StorageErr Error
    | Found a


type Error
    = JsonError Decode.Error


type alias Key =
    String


type alias Encoder a =
    a -> Value


config :
    { key : Key
    , encoder : Encoder a
    , decoder : Decoder a
    }
    -> Config a
config params =
    Config params.key params.encoder params.decoder


save : a -> Config a -> Cmd msg
save value (Config key encoder _) =
    Ports.send
        ( "Ports.LocalStorage.save"
        , Encode.object
            [ ( "key", Encode.string key )
            , ( "value", encoder value )
            ]
        )


requestLoad : Config a -> Cmd msg
requestLoad (Config key _ _) =
    Ports.send ( "Ports.LocalStorage.request", Encode.string key )


onLoad : (StorageResult a -> msg) -> Config a -> Ports.Listener msg
onLoad handler (Config key _ decoder) =
    let
        callback =
            Decode.decodeValue (Decode.maybe decoder)
                >> toStorageResult
                >> handler
    in
    ( "Ports.LocalStorage.listen." ++ key, callback )


toStorageResult : Result Decode.Error (Maybe a) -> StorageResult a
toStorageResult result =
    case result of
        Ok Nothing ->
            NotStored

        Ok (Just value) ->
            Found value

        Result.Err err ->
            StorageErr (JsonError err)
