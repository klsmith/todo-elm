module Todo.Urgency exposing
    ( Urgency(..)
    , compare
    )

-- TYPES


type Urgency
    = Whenever
    | Eventually
      -- | Deadline Posix
    | Soon
    | Asap



-- COMPARISONS


compare : Urgency -> Urgency -> Order
compare a b =
    Basics.compare
        (asIndex a)
        (asIndex b)


asIndex : Urgency -> Int
asIndex urg =
    case urg of
        Whenever ->
            0

        Eventually ->
            1

        -- Deadline _ ->
        --     2
        Soon ->
            3

        Asap ->
            4
