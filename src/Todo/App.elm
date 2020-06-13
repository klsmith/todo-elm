module Todo.App exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra exposing (Document, elText, toElementColor)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, placeholder)
import Parser exposing (DeadEnd)
import Ports.LocalStorage exposing (..)
import RemoteData exposing (RemoteData)
import Todo.Item as Item exposing (Item)



-- MODEL


type alias Model =
    { inputValue : Maybe Item
    }


type alias StoredData a =
    RemoteData Never a


type alias KeyValue =
    { key : String
    , value : Maybe String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputValue = Item.parse "need something asap"
      }
    , addLocalStorageListener storageKey
    )


storageKey : String
storageKey =
    "io.github.klsmith.todo-elm"



-- VIEW


bgColor : Element.Color
bgColor =
    toElementColor Color.darkCharcoal


textColor : Element.Color
textColor =
    toElementColor Color.white


impAcc : Element.Color
impAcc =
    toElementColor Color.lightPurple


urgAcc : Element.Color
urgAcc =
    toElementColor Color.blue


view : Model -> Document Msg
view model =
    { title = "Todo App"
    , options = []
    , attributes =
        [ Background.color bgColor
        , Font.color textColor
        ]
    , body =
        column [ centerX, centerY ]
            [ mainInput [] model.inputValue
            ]
    }


mainInput : List (Attribute Msg) -> Maybe Item -> Element Msg
mainInput attributes item =
    Input.multiline
        ([ Background.color bgColor
         ]
            ++ (Maybe.map (below << renderParsed []) item
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
            ++ attributes
        )
        { onChange = OnInputChange
        , text =
            Maybe.map Item.getRawText item
                |> Maybe.withDefault ""
        , placeholder = justPlaceholderText "add things to your todo list"
        , label = labelHidden "main input text box"
        , spellcheck = True
        }


justPlaceholderText =
    Just << placeholder [] << text


renderParsed : List (Attribute Msg) -> Item -> Element Msg
renderParsed attributes item =
    row [ paddingXY 8 16, spacing 8 ]
        [ renderImportance <| Item.getImportance item
        , renderUrgency <| Item.getUrgency item
        , text <| Debug.toString <| Item.getRawText item
        ]


renderImportance : Item.Importance -> Element msg
renderImportance importance =
    case importance of
        Item.Need ->
            elText
                [ Background.color (toElementColor Color.red)
                , Border.rounded 6
                , padding 4
                ]
                "NEED"

        Item.Want ->
            elText
                [ Background.color (toElementColor Color.orange)
                , Border.rounded 6
                , padding 4
                ]
                "WANT"

        Item.NoImportance ->
            elText
                [ Background.color (toElementColor Color.green)
                , Border.rounded 6
                , padding 4
                ]
                "NONE"


renderUrgency : Item.Urgency -> Element msg
renderUrgency urgency =
    case urgency of
        Item.Asap ->
            elText
                [ Background.color (toElementColor Color.red)
                , Border.rounded 6
                , padding 4
                ]
                "ASAP"

        Item.Deadline posix ->
            elText
                [ Background.color (toElementColor Color.orange)
                , Border.rounded 6
                , padding 4
                ]
                "DEADLINE: --/--/--"

        Item.Whenever ->
            elText
                [ Background.color (toElementColor Color.green)
                , Border.rounded 6
                , padding 4
                ]
                "WHENEVER"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onLocalStorageChange OnStorageChange



-- MESSAGES


type Msg
    = OnInputChange String
    | OnStorageChange KeyValue



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( OnInputChange newValue, _ ) ->
            ( { model
                | inputValue = Item.parse newValue
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )
