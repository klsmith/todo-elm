module Responsive exposing (Layout(..), layout, onResize)

import Element
    exposing
        ( Device
        , DeviceClass(..)
        , Orientation(..)
        )
import Json.Decode as Decode
import Ports
import Ports.Device as Device


type Layout
    = Mobile
    | Desktop


layout : { s | width : Int, height : Int } -> Layout
layout screen =
    let
        device =
            Element.classifyDevice screen
    in
    fromDevice device


fromDevice : Device -> Layout
fromDevice device =
    case ( device.class, device.orientation ) of
        ( Phone, Portrait ) ->
            Mobile

        ( _, _ ) ->
            Desktop


onResize : (Result Decode.Error Layout -> msg) -> Ports.Listener msg
onResize handler =
    Device.onResize
        (\r ->
            handler <|
                case r of
                    Ok device ->
                        Ok (fromDevice device)

                    Err err ->
                        Err err
        )
