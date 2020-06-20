module Todo.Item exposing
    ( Item
    , compare
    , decoder
    , encode
    , equals
    , getDetails
    , getImportance
    , getRawText
    , getUrgency
    , parse
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Token as Token exposing (Token)
import Todo.Urgency as Urgency exposing (Urgency(..))
import Util exposing (andThenCompareWith)


type Item
    = Item Importance Urgency String String


encode : Item -> Value
encode (Item imp urg det raw) =
    Encode.object
        [ ( "importance", Importance.encode imp )
        , ( "urgency", Urgency.encode urg )
        , ( "details", Encode.string det )
        , ( "rawText", Encode.string raw )
        ]


decoder : Decoder Item
decoder =
    Decode.map4 Item
        (Decode.field "importance" Importance.decoder)
        (Decode.field "urgency" Urgency.decoder)
        (Decode.field "details" Decode.string)
        (Decode.field "rawText" Decode.string)


compare : Item -> Item -> Order
compare itemA itemB =
    Importance.compare
        (getImportance itemA)
        (getImportance itemB)
        |> andThenCompareWith
            Urgency.compare
            (getUrgency itemA)
            (getUrgency itemB)
        |> andThenCompareWith
            Basics.compare
            (getDetails itemA)
            (getDetails itemB)


equals : Item -> Item -> Bool
equals itemA itemB =
    compare itemA itemB == EQ


getImportance : Item -> Importance
getImportance (Item imp _ _ _) =
    imp


getUrgency : Item -> Urgency
getUrgency (Item _ urg _ _) =
    urg


getDetails : Item -> String
getDetails (Item _ _ details _) =
    details


getRawText : Item -> String
getRawText (Item _ _ _ rawText) =
    rawText


parse : String -> Maybe Item
parse =
    parseTokens << Token.tokenize


parseTokens : List Token -> Maybe Item
parseTokens tokens =
    let
        imp =
            List.filterMap Token.toImportance tokens
                |> List.head
                |> Maybe.withDefault NoImportance

        urg =
            List.filterMap Token.toUrgency tokens
                |> List.head
                |> Maybe.withDefault Whenever

        txts =
            List.filterMap Token.toText tokens

        rawText =
            List.map Token.toString tokens
                |> String.join " "
    in
    if List.length txts == 1 then
        Just (Item imp urg (String.concat txts) rawText)

    else if String.isEmpty rawText then
        Nothing

    else
        Just (Item imp urg rawText rawText)
