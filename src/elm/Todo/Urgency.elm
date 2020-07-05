module Todo.Urgency exposing
    ( Urgency(..)
    , toDisplayString
    )


type Urgency
    = Whenever
    | Eventually
      -- | Deadline Posix
    | Soon
    | Asap


toDisplayString : Urgency -> String
toDisplayString urg =
    case urg of
        Whenever ->
            "WHENEVER"

        Eventually ->
            "EVENTUALLY"

        Soon ->
            "SOON"

        Asap ->
            "ASAP"
