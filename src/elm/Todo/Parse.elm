module Todo.Parse exposing
    ( Token(..)
    , item
    )

import Todo.Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Urgency exposing (Urgency(..))



-- PARSING


item : String -> Item
item string =
    let
        tokens =
            tokenize string

        imp =
            tokens
                |> List.filterMap importanceFromToken
                |> List.head
                |> Maybe.withDefault NoImportance

        urg =
            tokens
                |> List.filterMap urgencyFromToken
                |> List.head
                |> Maybe.withDefault Whenever

        txts =
            tokens
                |> List.filterMap textFromToken

        rawText =
            tokens
                |> List.map tokenToString
                |> String.join " "
    in
    if List.length txts == 1 then
        Item.create imp urg (String.concat txts) rawText

    else
        Item.create imp urg rawText rawText


importance : String -> Maybe Importance
importance string =
    case
        string
            |> String.filter Char.isAlphaNum
            |> String.toUpper
    of
        "NEED" ->
            Just Need

        "WANT" ->
            Just Want

        _ ->
            Nothing


urgency : String -> Maybe Urgency
urgency string =
    case
        string
            |> String.filter Char.isAlphaNum
            |> String.toUpper
    of
        "ASAP" ->
            Just Asap

        "SOON" ->
            Just Soon

        "EVENTUALLY" ->
            Just Eventually

        "WHENEVER" ->
            Just Whenever

        _ ->
            Nothing



-- TOKENS


type Token
    = Txt String
    | Imp String Importance
    | Urg String Urgency


tokenize : String -> List Token
tokenize string =
    if String.contains " " string then
        String.split " " string
            |> List.map tokenize
            |> List.concat
            |> List.reverse
            |> List.foldr aggregateTokens []
            |> List.reverse

    else
        case
            ( importance string
            , urgency string
            )
        of
            ( Just imp, _ ) ->
                [ Imp string imp ]

            ( Nothing, Just urg ) ->
                [ Urg string urg ]

            ( Nothing, Nothing ) ->
                [ Txt string ]


aggregateTokens : Token -> List Token -> List Token
aggregateTokens token list =
    let
        maybeEnd =
            List.head list

        rest =
            List.tail list |> Maybe.withDefault []
    in
    case maybeEnd of
        Nothing ->
            token :: list

        Just end ->
            case ( end, token ) of
                ( Txt "", Txt "" ) ->
                    list

                ( Imp is imp, Txt "to" ) ->
                    Imp (String.join " " [ is, "to" ]) imp :: rest

                ( Txt "really", Imp is imp ) ->
                    Imp (String.join " " [ "really", is ]) imp :: rest

                ( Txt es, Txt ts ) ->
                    Txt (String.join " " [ es, ts ]) :: rest

                ( _, _ ) ->
                    token :: list



-- TOKEN HELPERS


importanceFromToken : Token -> Maybe Importance
importanceFromToken t =
    case t of
        Imp _ i ->
            Just i

        _ ->
            Nothing


urgencyFromToken : Token -> Maybe Urgency
urgencyFromToken t =
    case t of
        Urg _ u ->
            Just u

        _ ->
            Nothing


textFromToken : Token -> Maybe String
textFromToken t =
    case t of
        Txt s ->
            Just s

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
