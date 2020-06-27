module Ports.Device exposing (onResize)

import Element
import Json.Decode as Decode exposing (Decoder)
import Ports


type alias Screen =
    { width : Int, height : Int }


onResize : (Result Decode.Error Element.Device -> msg) -> Ports.Listener msg
onResize handler =
    ( "Ports.Device.onResize"
    , Decode.decodeValue screenDecoder
        >> Result.map Element.classifyDevice
        >> handler
    )


screenDecoder : Decoder Screen
screenDecoder =
    Decode.map2 Screen
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
