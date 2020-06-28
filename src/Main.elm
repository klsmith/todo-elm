module Main exposing (main)

import Color
    exposing
        ( Color
        , black
        , blue
        , charcoal
        , darkCharcoal
        , darkGreen
        , darkYellow
        , lightCharcoal
        , orange
        , red
        , white
        )
import Element as El
    exposing
        ( Attribute
        , Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        )
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
import Todo.Parse as Parse
import Todo.Save as Save
import Todo.Urgency as Urgency exposing (Urgency(..))
import Util exposing (tern)



-- MODEL


type alias Model =
    { inputValue : Maybe Item
    , items : List Item
    , device : Device
    }



-- INIT


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { inputValue = Nothing
      , items = []
      , device = El.classifyDevice flags
      }
    , LocalStorage.requestLoad storage
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


shadowStyle : El.Attr decorative msg
shadowStyle =
    ElBr.shadow
        { offset = ( 4, 8 )
        , size = 0
        , blur = 6
        , color = Elx.toElementColor black
        }


view : Model -> Document Msg
view model =
    let
        isMobile =
            case
                ( model.device.class
                , model.device.orientation
                )
            of
                ( Phone, Portrait ) ->
                    True

                ( _, _ ) ->
                    False
    in
    { title =
        "Todo App"
    , options =
        [ El.focusStyle
            { borderColor =
                Just
                    (Elx.toElementColor lightCharcoal)
            , backgroundColor =
                Just
                    (Elx.toElementColor darkCharcoal)
            , shadow = Nothing
            }
        ]
    , attributes =
        [ Elx.backgroundColor darkCharcoal
        , Elx.fontColor white
        , Elf.family
            [ Elf.typeface "Lucida Console"
            , Elf.typeface "Monaco"
            , Elf.monospace
            ]
        , Elf.size 16
        ]
            ++ (if isMobile then
                    [ El.inFront
                        (El.el
                            [ El.padding 8
                            , El.width El.fill
                            ]
                            (El.row
                                [ shadowStyle
                                , rounded
                                , El.width El.fill
                                ]
                                [ Eli.text
                                    ([ Elx.onEnter TriggerAddItem
                                     , Elx.backgroundColor charcoal
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
                                        ++ (model.inputValue
                                                |> Maybe.map
                                                    (El.below
                                                        << renderParsed []
                                                    )
                                                |> Maybe.map List.singleton
                                                |> Maybe.withDefault []
                                           )
                                    )
                                    { onChange = OnInputChange
                                    , text =
                                        model.inputValue
                                            |> Maybe.map Item.getRawText
                                            |> Maybe.withDefault ""
                                    , placeholder =
                                        Just
                                            (Elx.placeholder []
                                                "add things to your todo list"
                                            )
                                    , label =
                                        Eli.labelHidden
                                            "main input text box"
                                    }
                                , Eli.button
                                    [ Elx.backgroundColor darkGreen
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
                                        [ Elx.borderColor darkGreen
                                        , Elx.fontColor darkGreen
                                        , Elx.backgroundColor darkCharcoal
                                        ]
                                    ]
                                    { onPress = Just TriggerAddItem
                                    , label =
                                        Elx.text
                                            [ El.centerX
                                            , El.centerY
                                            , El.paddingXY 8 0
                                            ]
                                            "add"
                                    }
                                ]
                            )
                        )
                    ]

                else
                    []
               )
    , body =
        if isMobile then
            El.column
                [ El.paddingXY 8 70
                , El.spacing 16
                , El.width El.fill
                ]
                (model.items
                    |> List.map renderItemCard
                    |> List.reverse
                )

        else
            El.column
                [ El.width El.fill
                , El.height El.fill
                , El.spacing 16
                ]
                (El.row
                    [ El.centerX
                    , El.centerY
                    , El.width (El.minimum 480 El.shrink)
                    , shadowStyle
                    , rounded
                    ]
                    [ Eli.text
                        ([ Elx.onEnter TriggerAddItem
                         , Elx.backgroundColor charcoal
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
                            ++ (model.inputValue
                                    |> Maybe.map (El.onLeft << renderParsed [])
                                    |> Maybe.map List.singleton
                                    |> Maybe.withDefault []
                               )
                        )
                        { onChange = OnInputChange
                        , text =
                            model.inputValue
                                |> Maybe.map Item.getRawText
                                |> Maybe.withDefault ""
                        , placeholder =
                            Just
                                (Elx.placeholder []
                                    "add things to your todo list"
                                )
                        , label =
                            Eli.labelHidden
                                "main input text box"
                        }
                    , Eli.button
                        [ Elx.backgroundColor darkGreen
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
                            [ Elx.borderColor darkGreen
                            , Elx.fontColor darkGreen
                            , Elx.backgroundColor darkCharcoal
                            ]
                        ]
                        { onPress = Just TriggerAddItem
                        , label =
                            Elx.text
                                [ El.centerX
                                , El.centerY
                                , El.paddingXY 8 0
                                ]
                                "add"
                        }
                    ]
                    :: (model.items
                            |> List.map renderItem
                            |> List.reverse
                       )
                )
    }


renderItemCard : Item -> Element Msg
renderItemCard item =
    let
        pad =
            6
    in
    El.row
        [ El.width El.fill
        , El.height El.shrink
        , rounded
        , shadowStyle
        , Elx.backgroundColor blue
        , El.clip
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
            [ El.wrappedRow [ El.width El.fill, El.spacing pad ]
                [ importanceBadge [] (Item.getImportance item)
                , urgencyBadge [] (Item.getUrgency item)
                ]
            , El.paragraph
                (Elx.css
                    [ ( "-ms-word-break", "break-all" )
                    , ( "word-break", "break-all" )
                    , ( "word-break", "break-word" )
                    , ( "-webkit-hyphens", "auto" )
                    , ( "-moz-hyphens", "auto" )
                    , ( "hyphens", "auto" )
                    ]
                )
                [ El.text
                    (Item.getDetails item)
                ]
            ]
        , Eli.button
            [ El.width (El.fillPortion 1)
            , El.height El.fill
            , Elx.backgroundColor red
            , Elx.borderColor transparent
            , ElBr.width 2
            , roundEach
                { topRight = True
                , bottomRight = True
                , bottomLeft = False
                , topLeft = False
                }
            , El.focused
                [ Elx.borderColor red
                , Elx.fontColor red
                , Elx.backgroundColor darkCharcoal
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
        , Eli.button
            [ Elx.backgroundColor red
            , ElBr.rounded 2
            , ElBr.width 2
            , Elx.borderColor transparent
            , El.paddingXY 4 0
            , shadowStyle
            , El.focused
                [ Elx.borderColor red
                , Elx.fontColor red
                , Elx.backgroundColor darkCharcoal
                ]
            ]
            { onPress = Just (TriggerRemoveItem item)
            , label = El.text "X"
            }
        ]


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



-- BADGE RENDERING


importanceBadge : List (Attribute Msg) -> Importance -> Element Msg
importanceBadge attrs importance =
    let
        ( color, text ) =
            case importance of
                Need ->
                    ( red, "NEED" )

                Want ->
                    ( orange, "WANT" )

                NoImportance ->
                    ( darkGreen, "NOT IMPORTANT" )
    in
    badge (Elx.backgroundColor color :: attrs) text


urgencyBadge : List (Attribute Msg) -> Urgency -> Element Msg
urgencyBadge attrs urgency =
    let
        ( color, text ) =
            case urgency of
                Whenever ->
                    ( darkGreen, "WHENEVER" )

                Eventually ->
                    ( darkYellow, "EVENTUALLY" )

                -- Deadline _ ->
                --     ( orange, "DEADLINE ???" )
                Soon ->
                    ( orange, "SOON" )

                Asap ->
                    ( red, "ASAP" )
    in
    badge (Elx.backgroundColor color :: attrs) text


detailsBadge : List (Attribute Msg) -> String -> Element Msg
detailsBadge attrs =
    badge (Elx.backgroundColor blue :: attrs)


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
        [ Ports.listen OnBadPortMsg
            [ LocalStorage.onLoad OnLocalStorageLoad storage
            , Device.onResize OnDeviceResize
            ]
        ]



-- MESSAGES


type Msg
    = OnBadPortMsg String
    | OnInputChange String
    | TriggerAddItem
    | TriggerRemoveItem Item
    | OnLocalStorageLoad (StorageResult Save.Format)
    | OnDeviceResize (Result Decode.Error Device)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnBadPortMsg badMsg ->
            ( model
            , Log.string ("ELM received junk msg from JS: " ++ badMsg)
            )

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
            , newModel.items |> save
            )

        TriggerRemoveItem item ->
            let
                newModel =
                    removeItem item model
            in
            ( newModel
            , newModel.items |> save
            )

        OnLocalStorageLoad (Found format) ->
            ( { model | items = Save.deformat format }
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
            , Cmd.none
            )

        OnDeviceResize (Err err) ->
            ( model
            , Log.string
                ("Error Resizing Window: "
                    ++ Decode.errorToString err
                )
            )



-- UPDATE UTILITY


save : List Item -> Cmd Msg
save items =
    LocalStorage.save (Save.format items) storage


updateInput : String -> Model -> Model
updateInput string model =
    -- that's my secret... I'M ALWAYS PARSING!
    -- no, but really, this is what lets us have all the dynamic
    -- updates to the screen while typing!
    { model | inputValue = Parse.item string }


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
    let
        -- only allows items that do NOT match the given item through the filter
        newItems =
            model.items |> List.filter (not << Item.equals item)
    in
    { model | items = newItems }



-- MAIN


main : Program Flags Model Msg
main =
    Elx.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
