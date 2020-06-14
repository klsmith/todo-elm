module Main exposing (main)

import Color
import Element as El exposing (Attribute, Element)
import Element.Border as ElBdr
import Element.Extra as Elx exposing (Document)
import Element.Input as Eli exposing (Placeholder)
import Ports.LocalStorage exposing (addLocalStorageListener, onLocalStorageChange)
import Todo.Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Token as Token exposing (Token(..))
import Todo.Urgency exposing (Urgency(..))



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
        [ El.focusStyle
            { borderColor = Just <| Elx.toElementColor Color.lightCharcoal
            , backgroundColor = Just <| Elx.toElementColor Color.darkCharcoal
            , shadow = Nothing
            }
        ]
    , attributes =
        [ Elx.backgroundColor bgColor
        , Elx.fontColor textColor
        ]
    , body =
        El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing 16
            ]
            (mainInput [] model.inputValue
                :: List.reverse (List.map renderItem model.items)
            )
    }


renderItem : Item -> Element msg
renderItem item =
    El.row
        [ El.paddingXY 8 0
        , El.spacing 8
        , El.centerX
        , El.centerY
        , El.width El.shrink
        ]
        [ importanceBadge (Item.getImportance item)
        , urgencyBadge (Item.getUrgency item)
        , badge Color.blue (Item.getDetails item)
        ]


badge : Color.Color -> String -> Element msg
badge color string =
    Elx.elText
        [ Elx.backgroundColor color
        , ElBdr.rounded 6
        , El.padding 4
        , El.centerY
        , myShadow
        ]
        string


myShadow : El.Attr decorative msg
myShadow =
    ElBdr.shadow
        { offset = ( 4, 8 )
        , size = 0
        , blur = 6
        , color = Elx.toElementColor Color.black
        }


mainInput : List (Attribute Msg) -> Maybe Item -> Element Msg
mainInput attributes item =
    El.row
        [ El.centerX
        , El.centerY
        , El.width El.shrink
        , myShadow
        , ElBdr.rounded 6
        ]
        [ Eli.text
            ([ Elx.onEnter TriggerAddItem
             , Elx.backgroundColor Color.charcoal
             , El.width (El.fillPortion 5)
             , ElBdr.widthEach
                { left = 2
                , top = 2
                , bottom = 2
                , right = 1
                }
             , Elx.borderColor (Color.rgba 0 0 0 0)
             , ElBdr.roundEach
                { topLeft = 6
                , bottomLeft = 6
                , topRight = 0
                , bottomRight = 0
                }
             ]
                ++ (Maybe.map (El.onLeft << renderParsed []) item
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )
                -- ++ (Maybe.map (El.above << debugTokens) item
                --         |> Maybe.map List.singleton
                --         |> Maybe.withDefault []
                --    )
                ++ attributes
            )
            { onChange = OnInputChange
            , text =
                Maybe.map Item.getRawText item
                    |> Maybe.withDefault ""
            , placeholder = justPlaceholderText "add things to your todo list"
            , label = Eli.labelHidden "main input text box"
            }
        , Eli.button
            [ Elx.backgroundColor Color.darkGreen
            , ElBdr.widthEach
                { left = 1
                , top = 2
                , bottom = 2
                , right = 2
                }
            , Elx.borderColor (Color.rgba 0 0 0 0)
            , ElBdr.roundEach { topLeft = 0, topRight = 6, bottomLeft = 0, bottomRight = 6 }
            , El.width (El.fillPortion 1)
            , El.height El.fill
            , El.focused
                [ Elx.borderColor Color.darkGreen
                , Elx.fontColor Color.darkGreen
                , Elx.backgroundColor Color.darkCharcoal
                ]
            ]
            { onPress = Just TriggerAddItem
            , label = Elx.elText [ El.centerX, El.centerY, El.paddingXY 8 0 ] "add"
            }
        ]


debugTokens : Item -> Element msg
debugTokens item =
    El.row [] (List.map debugToken (Token.tokenize (Item.getRawText item)))


debugToken : Token -> Element msg
debugToken token =
    case token of
        Imp _ imp ->
            importanceBadge imp

        Urg _ urg ->
            urgencyBadge urg

        Txt txt ->
            badge Color.blue txt


justPlaceholderText : String -> Maybe (Placeholder msg)
justPlaceholderText =
    Just << Eli.placeholder [ Elx.fontColor <| Color.lightCharcoal ] << El.text


renderParsed : List (Attribute Msg) -> Item -> Element Msg
renderParsed attributes item =
    El.row
        ([ El.paddingXY 8 0
         , El.spacing 8
         , El.height El.fill
         ]
            ++ attributes
        )
        [ importanceBadge <| Item.getImportance item
        , urgencyBadge <| Item.getUrgency item
        ]


importanceBadge : Importance -> Element msg
importanceBadge importance =
    case importance of
        Need ->
            badge Color.red "NEED"

        Want ->
            badge Color.orange "WANT"

        NoImportance ->
            badge Color.darkGreen "NOT IMPORTANT"


urgencyBadge : Urgency -> Element msg
urgencyBadge urgency =
    case urgency of
        Asap ->
            badge Color.red "ASAP"

        Soon ->
            badge Color.orange "SOON"

        Deadline _ ->
            badge Color.orange "DEADLINE: --/--/--"

        Eventually ->
            badge Color.darkYellow "EVENTUALLY"

        Whenever ->
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


main : Program () Model Msg
main =
    Elx.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
