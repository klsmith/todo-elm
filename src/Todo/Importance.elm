module Todo.Importance exposing
    ( Importance(..)
    , compare
    , parse
    )


type Importance
    = NoImportance
    | Want
    | Need


compare : Importance -> Importance -> Order
compare a b =
    Basics.compare
        (asIndex a)
        (asIndex b)


asIndex : Importance -> Int
asIndex imp =
    case imp of
        NoImportance ->
            0

        Want ->
            1

        Need ->
            2


parse : String -> Maybe Importance
parse string =
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
