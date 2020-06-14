module Todo.Token exposing
    ( Token(..)
    , toImportance
    , toString
    , toText
    , toUrgency
    , tokenize
    )

import Todo.Importance as Importance exposing (Importance)
import Todo.Urgency as Urgency exposing (Urgency)


type Token
    = Txt String
    | Imp String Importance
    | Urg String Urgency


toImportance : Token -> Maybe Importance
toImportance t =
    case t of
        Imp _ i ->
            Just i

        _ ->
            Nothing


toUrgency : Token -> Maybe Urgency
toUrgency t =
    case t of
        Urg _ u ->
            Just u

        _ ->
            Nothing


toText : Token -> Maybe String
toText t =
    case t of
        Txt s ->
            Just s

        _ ->
            Nothing


toString : Token -> String
toString token =
    case token of
        Txt string ->
            string

        Imp string _ ->
            string

        Urg string _ ->
            string


tokenize : String -> List Token
tokenize string =
    if String.contains " " string then
        String.split " " string
            |> List.map tokenize
            |> List.concat
            |> List.reverse
            |> List.foldr aggregate []
            |> List.reverse

    else
        case
            ( Importance.parse string
            , Urgency.parse string
            )
        of
            ( Just imp, _ ) ->
                [ Imp string imp ]

            ( Nothing, Just urg ) ->
                [ Urg string urg ]

            ( Nothing, Nothing ) ->
                [ Txt string ]


aggregate : Token -> List Token -> List Token
aggregate token list =
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
            case ( token, end ) of
                ( Txt "", Txt "" ) ->
                    list

                ( Txt "to", Imp is imp ) ->
                    Imp (String.join " " [ is, "to" ]) imp :: rest

                ( Txt ts, Txt es ) ->
                    Txt (String.join " " [ es, ts ]) :: rest

                ( _, _ ) ->
                    token :: list
