module Main exposing (main)

import Browser.Events
import Color exposing (Color)
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as ElBr
import Element.Extra as Elx exposing (Document)
import Element.Font as Elf
import Element.Input as Eli exposing (Placeholder)
import Json.Decode as Decode
import Ports
import Ports.Device as Device
import Ports.LocalStorage as LocalStorage exposing (StorageResult(..))
import Ports.Log as Log
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Save as Save
import Todo.Token as Token exposing (Token(..))
import Todo.Urgency as Urgency exposing (Urgency(..))
import Util exposing (applyTuple, tern)



-- MODEL


type alias Model =
    { inputValue : Maybe Item
    , items : List Item
    , device : Device
    }



-- INIT


type alias Flags =
    { screen :
        { width : Int
        , height : Int
        }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { inputValue = Nothing
      , items = []
      , device = El.classifyDevice flags.screen
      }
    , LocalStorage.request storage
    )


storage : LocalStorage.Config Save.Format
storage =
    LocalStorage.config
        { key = "io.github.klsmith.todo-elm2"
        , encoder = Save.encode
        , decoder = Save.decoder
        }



-- COLORS


transparent : Color
transparent =
    Color.rgba 0 0 0 0



-- VIEW


view : Model -> Document Msg
view model =
    case
        ( model.device.class
        , model.device.orientation
        )
    of
        ( Phone, Portrait ) ->
            mobile model

        ( _, _ ) ->
            desktop model


title : String
title =
    "Todo App"


bgColor : Attribute Msg
bgColor =
    Elx.backgroundColor Color.darkCharcoal


fontColor : Attribute Msg
fontColor =
    Elx.fontColor Color.white


fontFamily : Attribute Msg
fontFamily =
    Elf.family
        [ Elf.typeface "Lucida Console"
        , Elf.typeface "Monaco"
        , Elf.monospace
        ]


fontSize : Attribute Msg
fontSize =
    Elf.size 16


mobile : Model -> Document Msg
mobile model =
    { title = "Todo App"
    , options = [ El.focusStyle focusStyle ]
    , attributes =
        [ bgColor
        , fontColor
        , fontFamily
        , fontSize
        , El.inFront
            (El.el
                [ El.padding 8
                , El.width El.fill
                ]
                (mobileItemInput [ El.width El.fill ]
                    model.inputValue
                )
            )
        ]
    , body =
        El.column
            [ El.paddingXY 8 70
            , El.spacing 16
            , El.width El.fill
            ]
            (List.reverse (List.map renderItemCard model.items))
    }


mobileItemInput : List (Attribute Msg) -> Maybe Item -> Element Msg
mobileItemInput attrs item =
    El.row
        ([ shadowStyle
         , rounded
         ]
            ++ attrs
        )
        [ mobileTextBox item
        , mobileAddButton
        ]


mobileTextBox : Maybe Item -> Element Msg
mobileTextBox item =
    Eli.text
        ([ Elx.onEnter TriggerAddItem
         , Elx.backgroundColor Color.charcoal
         , El.width (El.fillPortion 5)
         , ElBr.widthEach
            { left = 2
            , top = 2
            , bottom = 2
            , right = 1
            }
         , Elx.borderColor transparent
         , roundLeftSideOnly
         ]
            ++ (Maybe.map (El.below << renderParsed []) item
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
        )
        { onChange = OnInputChange
        , text =
            Maybe.map Item.getRawText item
                |> Maybe.withDefault ""
        , placeholder = justPlaceholderText "add things to your todo list"
        , label = Eli.labelHidden "main input text box"
        }


mobileAddButton : Element Msg
mobileAddButton =
    Eli.button
        [ Elx.backgroundColor Color.darkGreen
        , ElBr.widthEach
            { left = 1
            , top = 2
            , bottom = 2
            , right = 2
            }
        , Elx.borderColor transparent
        , roundRightSideOnly
        , El.width (El.fillPortion 1)
        , El.height El.fill
        , El.focused
            [ Elx.borderColor Color.darkGreen
            , Elx.fontColor Color.darkGreen
            , Elx.backgroundColor Color.darkCharcoal
            ]
        ]
        { onPress = Just TriggerAddItem
        , label = Elx.text [ El.centerX, El.centerY, El.paddingXY 8 0 ] "add"
        }


renderItemCard : Item -> Element Msg
renderItemCard item =
    let
        pad =
            6

        ( importanceColor, importanceText ) =
            Importance.getDisplayData (Item.getImportance item)

        ( urgencyColor, urgencyText ) =
            Urgency.getDisplayData (Item.getUrgency item)
    in
    El.row
        [ El.width El.fill
        , El.height El.shrink
        , rounded
        , shadowStyle
        , Elx.backgroundColor Color.blue
        ]
        [ El.column
            [ El.width (El.fillPortion 6)
            , El.padding pad
            , El.spacing pad
            , roundEach
                { topLeft = True
                , bottomLeft = True
                , topRight = False
                , bottomRight = False
                }
            ]
            [ El.row [ El.width El.fill, El.spacing pad ]
                [ importanceBadge [] (Item.getImportance item)
                , urgencyBadge [] (Item.getUrgency item)
                ]
            , El.paragraph []
                [ El.text
                    (Item.getDetails item)
                ]
            ]
        , Eli.button
            [ El.width (El.fillPortion 1)
            , El.height El.fill
            , Elx.backgroundColor Color.red
            , Elx.borderColor transparent
            , ElBr.width 2
            , roundEach
                { topRight = True
                , bottomRight = True
                , bottomLeft = False
                , topLeft = False
                }
            , El.focused
                [ Elx.borderColor Color.red
                , Elx.fontColor Color.red
                , Elx.backgroundColor Color.darkCharcoal
                ]
            ]
            { onPress = Just (TriggerRemoveItem item)
            , label =
                Elx.text
                    [ El.centerX
                    , El.centerY
                    ]
                    "X"
            }
        ]


desktop : Model -> Document Msg
desktop model =
    { title = "Todo App"
    , options = [ El.focusStyle focusStyle ]
    , attributes =
        [ bgColor
        , fontColor
        , fontFamily
        , fontSize
        ]
    , body =
        El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing 16
            ]
            (itemInput
                [ El.centerX
                , El.centerY
                , El.width (El.minimum 480 El.shrink)
                ]
                model.inputValue
                :: List.reverse (List.map renderItem model.items)
            )
    }


focusStyle : El.FocusStyle
focusStyle =
    { borderColor = Just (Elx.toElementColor Color.lightCharcoal)
    , backgroundColor = Just (Elx.toElementColor Color.darkCharcoal)
    , shadow = Nothing
    }


shadowStyle : El.Attr decorative msg
shadowStyle =
    ElBr.shadow
        { offset = ( 4, 8 )
        , size = 0
        , blur = 6
        , color = Elx.toElementColor Color.black
        }


itemInput : List (Attribute Msg) -> Maybe Item -> Element Msg
itemInput attrs item =
    El.row
        ([ shadowStyle
         , rounded
         ]
            ++ attrs
        )
        [ textBox item
        , addButton
        ]


textBox : Maybe Item -> Element Msg
textBox item =
    Eli.text
        ([ Elx.onEnter TriggerAddItem
         , Elx.backgroundColor Color.charcoal
         , El.width (El.fillPortion 5)
         , ElBr.widthEach
            { left = 2
            , top = 2
            , bottom = 2
            , right = 1
            }
         , Elx.borderColor transparent
         , roundLeftSideOnly
         ]
            ++ (Maybe.map (El.onLeft << renderParsed []) item
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
        )
        { onChange = OnInputChange
        , text =
            Maybe.map Item.getRawText item
                |> Maybe.withDefault ""
        , placeholder = justPlaceholderText "add things to your todo list"
        , label = Eli.labelHidden "main input text box"
        }


addButton : Element Msg
addButton =
    Eli.button
        [ Elx.backgroundColor Color.darkGreen
        , ElBr.widthEach
            { left = 1
            , top = 2
            , bottom = 2
            , right = 2
            }
        , Elx.borderColor transparent
        , roundRightSideOnly
        , El.width (El.fillPortion 1)
        , El.height El.fill
        , El.focused
            [ Elx.borderColor Color.darkGreen
            , Elx.fontColor Color.darkGreen
            , Elx.backgroundColor Color.darkCharcoal
            ]
        ]
        { onPress = Just TriggerAddItem
        , label = Elx.text [ El.centerX, El.centerY, El.paddingXY 8 0 ] "add"
        }


renderItem : Item -> Element Msg
renderItem item =
    El.row
        [ El.paddingXY 8 0
        , El.spacing 8
        , El.centerX
        , El.centerY
        , El.width El.shrink
        ]
        [ importanceBadge [ shadowStyle ] (Item.getImportance item)
        , urgencyBadge [ shadowStyle ] (Item.getUrgency item)
        , detailsBadge [ shadowStyle ] (Item.getDetails item)
        , removeButton item
        ]


removeButton : Item -> Element Msg
removeButton item =
    Eli.button
        [ Elx.backgroundColor Color.red
        , ElBr.rounded 2
        , ElBr.width 2
        , Elx.borderColor transparent
        , El.paddingXY 4 0
        , shadowStyle
        , El.focused
            [ Elx.borderColor Color.red
            , Elx.fontColor Color.red
            , Elx.backgroundColor Color.darkCharcoal
            ]
        ]
        { onPress = Just (TriggerRemoveItem item)
        , label = El.text "X"
        }


justPlaceholderText : String -> Maybe (Placeholder msg)
justPlaceholderText =
    Just
        << Eli.placeholder [ Elx.fontColor Color.lightCharcoal ]
        << El.text


renderParsed : List (Attribute Msg) -> Item -> Element Msg
renderParsed attributes item =
    El.row
        ([ El.paddingXY 8 0
         , El.spacing 8
         , El.height El.fill
         ]
            ++ attributes
        )
        [ importanceBadge [ shadowStyle ] (Item.getImportance item)
        , urgencyBadge [ shadowStyle ] (Item.getUrgency item)
        ]


importanceBadge : List (Attribute Msg) -> Importance -> Element Msg
importanceBadge attrs importance =
    let
        ( color, text ) =
            Importance.getDisplayData importance
    in
    badge ([ Elx.backgroundColor color ] ++ attrs) text


urgencyBadge : List (Attribute Msg) -> Urgency -> Element Msg
urgencyBadge attrs urgency =
    let
        ( color, text ) =
            Urgency.getDisplayData urgency
    in
    badge ([ Elx.backgroundColor color ] ++ attrs) text


detailsBadge : List (Attribute Msg) -> String -> Element Msg
detailsBadge attrs =
    badge ([ Elx.backgroundColor Color.blue ] ++ attrs)


badge : List (Attribute Msg) -> String -> Element Msg
badge attrs string =
    Elx.text
        ([ rounded
         , El.padding 4
         , El.centerY
         ]
            ++ attrs
        )
        string



-- ROUNDING CONSISTENCY


prefRounded : Int
prefRounded =
    6


rounded : Attribute msg
rounded =
    ElBr.rounded prefRounded


roundEach :
    { topLeft : Bool
    , bottomLeft : Bool
    , topRight : Bool
    , bottomRight : Bool
    }
    -> Attribute msg
roundEach { topLeft, bottomLeft, topRight, bottomRight } =
    ElBr.roundEach
        { topLeft = topLeft |> tern ( prefRounded, 0 )
        , bottomLeft = bottomLeft |> tern ( prefRounded, 0 )
        , topRight = topRight |> tern ( prefRounded, 0 )
        , bottomRight = bottomRight |> tern ( prefRounded, 0 )
        }


roundRightSideOnly : Attribute msg
roundRightSideOnly =
    roundEach
        { topLeft = False
        , bottomLeft = False
        , topRight = True
        , bottomRight = True
        }


roundLeftSideOnly : Attribute msg
roundLeftSideOnly =
    roundEach
        { topLeft = True
        , bottomLeft = True
        , topRight = False
        , bottomRight = False
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.listen Noop
            [ LocalStorage.listener OnLocalStorageLoad storage
            , Device.onResize OnDeviceResize
            ]
        ]



-- MESSAGES


type Msg
    = Noop
    | OnInputChange String
    | TriggerAddItem
    | TriggerRemoveItem Item
    | OnLocalStorageLoad (StorageResult Save.Format)
    | OnDeviceResize (Result Decode.Error Device)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        OnInputChange string ->
            ( updateInput string model
            , Cmd.none
            )

        TriggerAddItem ->
            let
                newModel =
                    model
                        |> addItemFromInput
                        |> resetInput
            in
            ( newModel
            , saveCmd newModel.items
            )

        TriggerRemoveItem item ->
            let
                newModel =
                    removeItem item model
            in
            ( newModel
            , saveCmd newModel.items
            )

        OnLocalStorageLoad (Found save) ->
            ( { model | items = Save.deformat save }
            , Cmd.none
            )

        OnLocalStorageLoad NotStored ->
            ( { model | items = [] }
            , Log.string "I couldn't find a save!"
            )

        OnLocalStorageLoad (StorageErr (LocalStorage.JsonError err)) ->
            ( model
            , Log.string
                ("Error Loading Save: "
                    ++ Decode.errorToString err
                )
            )

        OnDeviceResize (Ok device) ->
            ( { model | device = device }
            , Log.string ("NEW DEVICE: " ++ Debug.toString device)
            )

        OnDeviceResize (Err err) ->
            ( model
            , Log.string
                ("Error Resizing Window: "
                    ++ Decode.errorToString err
                )
            )



-- UPDATE UTILITY


saveCmd : List Item -> Cmd Msg
saveCmd items =
    LocalStorage.save (Save.format items) storage


updateInput : String -> Model -> Model
updateInput string model =
    -- that's my secret... I'M ALWAYS PARSING!
    -- no, but really, this is what lets us have all the dynamic
    -- updates to the screen while typing!
    { model | inputValue = Item.parse string }


addItemFromInput : Model -> Model
addItemFromInput model =
    case model.inputValue of
        Just item ->
            { model
                | items =
                    if model.items |> contains item then
                        model.items

                    else
                        List.sortWith Item.compare
                            (item :: model.items)
            }

        Nothing ->
            model


contains : Item -> List Item -> Bool
contains item =
    List.any (Item.equals item)


resetInput : Model -> Model
resetInput model =
    -- since we are always parsing on update, we reset this to nothing;
    -- there is nothing we could parse to populate it accurately.
    { model | inputValue = Nothing }


removeItem : Item -> Model -> Model
removeItem item model =
    -- only allows items that do NOT match the given item through the filter
    { model | items = model.items |> List.filter (not << Item.equals item) }



-- MAIN


main : Program Flags Model Msg
main =
    Elx.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
