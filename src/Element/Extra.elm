module Element.Extra exposing
    ( Document
    , backgroundColor
    , borderColor
    , css
    , document
    , fontColor
    , mapDocument
    , onEnter
    , text
    , toElementColor
    )

import Browser
import Browser.Events
import Color exposing (Color)
import Element exposing (Attr, Attribute, Element, Option, layoutWith)
import Element.Background
import Element.Border
import Element.Font
import Html.Attributes
import Html.Events
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


text : List (Attribute msg) -> String -> Element msg
text attrs string =
    Element.el attrs <| Element.text string


fontColor : Color -> Attr decorative msg
fontColor =
    Element.Font.color << toElementColor


borderColor : Color -> Attr decorative msg
borderColor =
    Element.Border.color << toElementColor


backgroundColor : Color -> Attr decorative msg
backgroundColor =
    Element.Background.color << toElementColor


toElementColor : Color -> Element.Color
toElementColor color =
    Color.toRgba color
        |> (\{ red, green, blue, alpha } ->
                Element.rgba red green blue alpha
           )


attributes :
    List (Element.Attribute msg)
    -> List (Maybe (Element.Attribute msg))
    -> List (Element.Attribute msg)
attributes required optional =
    required ++ List.filterMap identity optional


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


css : List ( String, String ) -> List (Attribute msg)
css =
    List.map
        (\( name, value ) ->
            Element.htmlAttribute
                (Html.Attributes.style name value)
        )
