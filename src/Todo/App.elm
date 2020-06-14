module Todo.App exposing (Model, Msg, init, subscriptions, update, view)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra exposing (Document, backgroundColor, borderColor, elText, fontColor, onEnter, toElementColor)
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


bgColor : Color.Color
bgColor =
    Color.darkCharcoal


textColor : Color.Color
textColor =
    Color.white


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
        [ backgroundColor bgColor
        , fontColor textColor
        ]
    , body =
        column
            [ width fill
            , height fill
            , spacing 16
            ]
            ([ mainInput [] model.inputValue
             ]
                ++ List.reverse (List.map renderItem model.items)
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
        [ importanceBadge (Item.getImportance item)
        , urgencyBadge (Item.getUrgency item)
        , badge Color.blue (Item.getRawText item)
        ]


badge : Color.Color -> String -> Element msg
badge color string =
    elText
        [ backgroundColor color
        , Border.rounded 6
        , padding 4
        , centerY
        , myShadow
        ]
        string


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
             , backgroundColor Color.charcoal
             , width (fillPortion 5)
             , Border.widthEach
                { left = 2
                , top = 2
                , bottom = 2
                , right = 1
                }
             , borderColor (Color.rgba 0 0 0 0)
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
            [ backgroundColor Color.darkGreen
            , Border.widthEach
                { left = 1
                , top = 2
                , bottom = 2
                , right = 2
                }
            , borderColor (Color.rgba 0 0 0 0)
            , Border.roundEach { topLeft = 0, topRight = 6, bottomLeft = 0, bottomRight = 6 }
            , width (fillPortion 1)
            , height fill
            , focused
                [ borderColor Color.darkGreen
                , fontColor Color.darkGreen
                , backgroundColor Color.darkCharcoal
                ]
            ]
            { onPress = Just TriggerAddItem
            , label = elText [ centerX, centerY, paddingXY 8 0 ] "add"
            }
        ]


justPlaceholderText : String -> Maybe (Input.Placeholder msg)
justPlaceholderText =
    Just << placeholder [ fontColor <| Color.lightCharcoal ] << text


renderParsed : List (Attribute Msg) -> Item -> Element Msg
renderParsed attributes item =
    row
        ([ paddingXY 8 0
         , spacing 8
         , height fill
         ]
            ++ attributes
        )
        [ importanceBadge <| Item.getImportance item
        , urgencyBadge <| Item.getUrgency item
        ]


importanceBadge : Item.Importance -> Element msg
importanceBadge importance =
    case importance of
        Item.Need ->
            badge Color.red "NEED"

        Item.Want ->
            badge Color.orange "WANT"

        Item.NoImportance ->
            badge Color.darkGreen "NOT IMPORTANT"


urgencyBadge : Item.Urgency -> Element msg
urgencyBadge urgency =
    case urgency of
        Item.Asap ->
            badge Color.red "ASAP"

        Item.Soon ->
            badge Color.orange "SOON"

        Item.Deadline _ ->
            badge Color.orange "DEADLINE: --/--/--"

        Item.Eventually ->
            badge Color.darkYellow "EVENTUALLY"

        Item.Whenever ->
            badge Color.darkGreen "WHENEVER"



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
                    List.sortWith Item.compare
                        ((Maybe.map List.singleton model.inputValue
                            |> Maybe.withDefault []
                         )
                            ++ model.items
                        )
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )
