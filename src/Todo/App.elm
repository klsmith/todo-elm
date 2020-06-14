module Todo.App exposing (Model, Msg, init, subscriptions, update, view)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra exposing (Document, elText, onEnter, toElementColor)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, placeholder)
import Ports.LocalStorage exposing (addLocalStorageListener, onLocalStorageChange)
import Todo.Item as Item exposing (Item)



-- MODEL


type alias Model =
    { inputValue : Maybe Item
    , items : List Item
    }


type alias KeyValue =
    { key : String
    , value : Maybe String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputValue = Item.parse "need something asap"
      , items = []
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
    , options =
        [ focusStyle
            { borderColor = Just <| toElementColor Color.lightCharcoal
            , backgroundColor = Just <| toElementColor Color.darkCharcoal
            , shadow = Nothing
            }
        ]
    , attributes =
        [ Background.color bgColor
        , Font.color textColor
        ]
    , body =
        column
            [ width fill
            , height fill
            , spacing 16
            ]
            ([ mainInput [] model.inputValue
             ]
                ++ List.map renderItem model.items
            )
    }


renderItem : Item -> Element msg
renderItem item =
    row
        [ paddingXY 8 0
        , spacing 8
        , centerX
        , centerY
        , width shrink
        ]
        [ renderImportance <| Item.getImportance item
        , renderUrgency <| Item.getUrgency item
        , elText
            [ Background.color (toElementColor Color.charcoal)
            , Border.rounded 6
            , padding 4
            , centerY
            , myShadow
            ]
            (Item.getRawText item)
        ]


myShadow =
    Border.shadow
        { offset = ( 4, 8 )
        , size = 0
        , blur = 6
        , color = toElementColor Color.black
        }


mainInput : List (Attribute Msg) -> Maybe Item -> Element Msg
mainInput attributes item =
    row
        [ centerX
        , centerY
        , width shrink
        , myShadow
        , Border.rounded 6
        ]
        [ Input.text
            ([ onEnter TriggerAddItem
             , Background.color (toElementColor Color.charcoal)
             , width (fillPortion 5)
             , Border.width 1
             , Border.color (Element.rgba 0 0 0 0)
             , Border.roundEach
                { topLeft = 6
                , bottomLeft = 6
                , topRight = 0
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
            [ Background.color (toElementColor Color.darkGreen)
            , Border.width 1
            , Border.color (Element.rgba 0 0 0 0)
            , Border.roundEach { topLeft = 0, topRight = 6, bottomLeft = 0, bottomRight = 6 }
            , width (fillPortion 1)
            , height fill
            ]
            { onPress = Just TriggerAddItem
            , label = elText [ centerX, centerY, paddingXY 8 0 ] "add"
            }
        ]


justPlaceholderText : String -> Maybe (Input.Placeholder msg)
justPlaceholderText =
    Just << placeholder [ Font.color <| toElementColor Color.lightCharcoal ] << text


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
                    ( Color.darkGreen, "NOT IMPORTANT" )
    in
    elText
        [ Background.color (toElementColor colorToUse)
        , Border.rounded 6
        , padding 4
        , centerY
        , myShadow
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
                    ( Color.darkGreen, "WHENEVER" )
    in
    elText
        [ Background.color (toElementColor colorToUse)
        , Border.rounded 6
        , padding 4
        , centerY
        , myShadow
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
    | TriggerAddItem



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

        ( TriggerAddItem, _ ) ->
            ( { model
                | inputValue = Nothing
                , items =
                    (Maybe.map List.singleton model.inputValue
                        |> Maybe.withDefault []
                    )
                        ++ model.items
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )
