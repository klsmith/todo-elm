module Todo.Importance exposing
    ( Importance(..)
    , toDisplayString
    )


type Importance
    = NoImportance
    | Want
    | Need


toDisplayString : Importance -> String
toDisplayString imp =
    case imp of
        NoImportance ->
            "NOT IMPORTANT"

        Want ->
            "WANT"

        Need ->
            "NEED"
