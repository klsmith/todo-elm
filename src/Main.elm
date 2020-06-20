module Main exposing (main)

import Color exposing (Color)
import Element as El exposing (Attribute, Element)
import Element.Border as ElBr
import Element.Extra as Elx exposing (Document)
import Element.Font as Elf
import Element.Input as Eli exposing (Placeholder)
import Ports
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
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputValue = Nothing
      , items = []
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
    { title = "Todo App"
    , options = [ El.focusStyle focusStyle ]
    , attributes =
        [ Elx.backgroundColor Color.darkCharcoal
        , Elx.fontColor Color.white
        , Elf.family
            [ Elf.typeface "Lucida Console"
            , Elf.typeface "Monaco"
            , Elf.monospace
            ]
        ]
    , body =
        El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing 16
            ]
            (mainInput model.inputValue
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


mainInput : Maybe Item -> Element Msg
mainInput item =
    El.row
        [ El.centerX
        , El.centerY
        , El.width (El.minimum 480 El.shrink)
        , shadowStyle
        , rounded
        ]
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
         -- ++ (Maybe.map (El.above << debugTokens) item
         --         |> Maybe.map List.singleton
         --         |> Maybe.withDefault []
         --    )
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
        , label = Elx.elText [ El.centerX, El.centerY, El.paddingXY 8 0 ] "add"
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
        [ importanceBadge (Item.getImportance item)
        , urgencyBadge (Item.getUrgency item)
        , badge Color.blue (Item.getDetails item)
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
        [ importanceBadge (Item.getImportance item)
        , urgencyBadge (Item.getUrgency item)
        ]


importanceBadge : Importance -> Element msg
importanceBadge importance =
    Importance.getDisplayData importance
        |> applyTuple badge


urgencyBadge : Urgency -> Element msg
urgencyBadge urgency =
    Urgency.getDisplayData urgency
        |> applyTuple badge


badge : Color.Color -> String -> Element msg
badge color string =
    Elx.elText
        [ Elx.backgroundColor color
        , rounded
        , El.padding 4
        , El.centerY
        , shadowStyle
        ]
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



-- DEBUG STUFF, NEED TO REMOVE!


debugTokens : Item -> Element msg
debugTokens item =
    El.row []
        (List.map debugToken
            (Token.tokenize (Item.getRawText item))
        )


debugToken : Token -> Element msg
debugToken token =
    case token of
        Imp _ imp ->
            importanceBadge imp

        Urg _ urg ->
            urgencyBadge urg

        Txt txt ->
            badge Color.blue txt



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.noopListener Noop
        |> LocalStorage.register OnLocalStorageLoad storage
        |> Ports.asSubscriptions



-- MESSAGES


type Msg
    = Noop
    | OnInputChange String
    | TriggerAddItem
    | TriggerRemoveItem Item
    | OnLocalStorageLoad (StorageResult Save.Format)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        OnInputChange string ->
            ( model |> updateInput string
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
            , storage |> LocalStorage.save (Save.V1 newModel.items)
            )

        TriggerRemoveItem item ->
            let
                newModel =
                    model |> removeItem item
            in
            ( newModel
            , storage |> LocalStorage.save (Save.V1 newModel.items)
            )

        OnLocalStorageLoad result ->
            case result of
                Found save ->
                    case save of
                        Save.V1 items ->
                            ( { model | items = items }, Cmd.none )

                NotStored ->
                    ( { model | items = [] }, Log.string "NotStored" )

                StorageErr err ->
                    ( model, Log.string ("Storage Error: " ++ Debug.toString err) )



-- UPDATE UTILITY


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
                    if model.items |> List.any (Item.equals item) then
                        model.items

                    else
                        List.sortWith Item.compare
                            (item :: model.items)
            }

        Nothing ->
            model


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


main : Program () Model Msg
main =
    Elx.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
