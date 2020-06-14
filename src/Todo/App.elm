module Todo.App exposing (Model, Msg, init, subscriptions, update, view)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra exposing (Document, elText, toElementColor)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, placeholder)
import Ports.LocalStorage exposing (addLocalStorageListener, onLocalStorageChange)
import Todo.Item as Item exposing (Item)



-- MODEL


type alias Model =
    { inputValue : Maybe Item
    }


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


view : Model -> Document Msg
view model =
    { title = "Todo App"
    , options = []
    , attributes =
        [ Background.color bgColor
        , Font.color textColor
        ]
    , body =
        column [ width fill, height fill ]
            [ mainInput [] model.inputValue
            ]
    }


mainInput : List (Attribute Msg) -> Maybe Item -> Element Msg
mainInput attributes item =
    row
        [ centerX
        , centerY
        , width shrink
        ]
        [ Input.text
            ([ Background.color bgColor
             , width (fillPortion 5)
             , Border.roundEach
                { topLeft = 6
                , topRight = 0
                , bottomLeft = 6
                , bottomRight = 0
                }
             ]
                ++ (Maybe.map (onLeft << renderParsed []) item
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
            }
        , Input.button
            [ Background.color (toElementColor Color.green)
            , Border.color (toElementColor Color.darkGray)
            , Border.widthEach { bottom = 1, left = 1, right = 1, top = 1 }
            , Border.roundEach { topLeft = 0, topRight = 6, bottomLeft = 0, bottomRight = 6 }
            , width (fillPortion 1)
            , height fill
            ]
            { onPress = Nothing
            , label = elText [ centerX, centerY, paddingXY 8 0 ] "add"
            }
        ]


justPlaceholderText : String -> Maybe (Input.Placeholder msg)
justPlaceholderText =
    Just << placeholder [] << text


renderParsed : List (Attribute Msg) -> Item -> Element Msg
renderParsed attributes item =
    row
        ([ paddingXY 8 0
         , spacing 8
         , height fill
         ]
            ++ attributes
        )
        [ renderImportance <| Item.getImportance item
        , renderUrgency <| Item.getUrgency item
        ]


renderImportance : Item.Importance -> Element msg
renderImportance importance =
    let
        ( colorToUse, textToDisplay ) =
            case importance of
                Item.Need ->
                    ( Color.red, "NEED" )

                Item.Want ->
                    ( Color.orange, "WANT" )

                Item.NoImportance ->
                    ( Color.green, "NOT IMPORTANT" )
    in
    elText
        [ Background.color (toElementColor colorToUse)
        , Border.rounded 6
        , padding 4
        , centerY
        ]
        textToDisplay


renderUrgency : Item.Urgency -> Element msg
renderUrgency urgency =
    let
        ( colorToUse, textToDisplay ) =
            case urgency of
                Item.Asap ->
                    ( Color.red, "ASAP" )

                Item.Deadline _ ->
                    ( Color.orange, "DEADLINE: --/--/--" )

                Item.Whenever ->
                    ( Color.green, "WHENEVER" )
    in
    elText
        [ Background.color (toElementColor colorToUse)
        , Border.rounded 6
        , padding 4
        , centerY
        ]
        textToDisplay



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
