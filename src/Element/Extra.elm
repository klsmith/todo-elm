module Element.Extra exposing
    ( Document
    , document
    , elText
    , mapDocument
    , onEnter
    , toElementColor
    )

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Color exposing (Color)
import Element exposing (Attribute, Element, Option, layoutWith)
import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Keyboard.Key as Key exposing (Key)


type alias Document msg =
    { title : String
    , options : List Option
    , attributes : List (Attribute msg)
    , body : Element msg
    }


document :
    { init : flags -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Document msg
    }
    -> Program flags model msg
document config =
    Browser.document
        { init = config.init
        , subscriptions = config.subscriptions
        , update = config.update
        , view = toHtmlView config.view
        }


toHtmlView : (model -> Document msg) -> (model -> Browser.Document msg)
toHtmlView view =
    \m -> view m |> toBrowserDocument


toBrowserDocument : Document msg -> Browser.Document msg
toBrowserDocument doc =
    { title = doc.title
    , body =
        [ layoutWith
            { options = doc.options }
            doc.attributes
            doc.body
        ]
    }


mapDocument : (subMsg -> msg) -> Document subMsg -> Document msg
mapDocument toMsg doc =
    { title = doc.title
    , options = doc.options
    , attributes = List.map (\a -> Element.mapAttribute toMsg a) doc.attributes
    , body = Element.map toMsg doc.body
    }


elText : List (Attribute msg) -> String -> Element msg
elText attrs string =
    Element.el attrs <| Element.text string


toElementColor : Color -> Element.Color
toElementColor color =
    Color.toRgba color
        |> (\{ red, green, blue, alpha } ->
                Element.rgba red green blue alpha
           )


onEnter : msg -> Attribute msg
onEnter msg =
    onlyForKey Key.Enter msg
        |> considerKeyboardEvent
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute


onlyForKey : Key -> msg -> KeyboardEvent -> Maybe msg
onlyForKey key msg keyEvent =
    if keyEvent.keyCode == key then
        Just msg

    else
        Nothing
