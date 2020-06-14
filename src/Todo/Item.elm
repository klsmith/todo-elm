module Todo.Item exposing
    ( Importance(..)
    , Item
    , RawText
    , Urgency(..)
    , compare
    , getImportance
    , getRawText
    , getUrgency
    , parse
    )

import Time exposing (Posix)


type Item
    = Item Importance Urgency RawText


type Importance
    = NoImportance
    | Want
    | Need


type Urgency
    = Whenever
    | Eventually
    | Deadline Posix
    | Soon
    | Asap


type alias RawText =
    String


type Token
    = Txt String
    | Imp String Importance
    | Urg String Urgency


compare : Item -> Item -> Order
compare itemA itemB =
    impCompare
        (getImportance itemA)
        (getImportance itemB)
        |> andThenWith urgCompare
            (getUrgency itemA)
            (getUrgency itemB)
        |> andThenWith Basics.compare
            (getRawText itemA)
            (getRawText itemB)


impCompare : Importance -> Importance -> Order
impCompare impA impB =
    Basics.compare
        (impToIndex impA)
        (impToIndex impB)


urgCompare : Urgency -> Urgency -> Order
urgCompare urgA urgB =
    Basics.compare
        (urgToIndex urgA)
        (urgToIndex urgB)


andThenWith : (a -> a -> Order) -> a -> a -> Order -> Order
andThenWith comparator a b higherOrder =
    if higherOrder == EQ then
        comparator a b

    else
        higherOrder


impToIndex : Importance -> Int
impToIndex imp =
    case imp of
        NoImportance ->
            0

        Want ->
            1

        Need ->
            2


urgToIndex : Urgency -> Int
urgToIndex urg =
    case urg of
        Whenever ->
            0

        Eventually ->
            1

        Deadline _ ->
            2

        Soon ->
            3

        Asap ->
            4


getRawText : Item -> String
getRawText (Item _ _ string) =
    string


getImportance : Item -> Importance
getImportance (Item imp _ _) =
    imp


getUrgency : Item -> Urgency
getUrgency (Item _ urg _) =
    urg


parse : String -> Maybe Item
parse string =
    parseTokens (tokenize string)


parseTokens : List Token -> Maybe Item
parseTokens tokens =
    let
        imp =
            List.filterMap extractImp tokens
                |> List.head
                |> Maybe.withDefault NoImportance

        urg =
            List.filterMap extractUrg tokens
                |> List.head
                |> Maybe.withDefault Whenever

        rawText =
            List.map tokenToString tokens
                |> String.join " "
    in
    if String.isEmpty rawText then
        Nothing

    else
        Just (Item imp urg rawText)


extractImp : Token -> Maybe Importance
extractImp t =
    case t of
        Imp _ i ->
            Just i

        _ ->
            Nothing


extractUrg : Token -> Maybe Urgency
extractUrg t =
    case t of
        Urg _ u ->
            Just u

        _ ->
            Nothing


tokenToString : Token -> String
tokenToString token =
    case token of
        Txt string ->
            string

        Imp string _ ->
            string

        Urg string _ ->
            string


tokenize : String -> List Token
tokenize string =
    let
        upperString =
            String.toUpper string
    in
    if String.contains " " string then
        String.split " " string
            |> List.map tokenize
            |> List.concat

    else if upperString == "NEED" then
        [ Imp string Need ]

    else if upperString == "WANT" then
        [ Imp string Want ]

    else if upperString == "ASAP" then
        [ Urg string Asap ]

    else if upperString == "SOON" then
        [ Urg string Soon ]

    else if upperString == "EVENTUALLY" then
        [ Urg string Eventually ]

    else if upperString == "WHENEVER" then
        [ Urg string Whenever ]

    else
        [ Txt string ]
