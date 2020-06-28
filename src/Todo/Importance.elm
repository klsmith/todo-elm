module Todo.Importance exposing
    ( Importance(..)
    , compare
    )

-- TYPES


type Importance
    = NoImportance
    | Want
    | Need



-- COMPARISONS


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
