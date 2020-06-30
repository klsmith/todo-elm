module Expect.Extra exposing (format, has)

import Expect exposing (Expectation)


has :
    String
    -> (b -> a)
    -> (a -> String)
    -> a
    -> Maybe b
    -> Expectation
has holderName getter toString expected maybe =
    case maybe of
        Just holder ->
            let
                actual =
                    getter holder
            in
            actual
                |> (==) expected
                |> Expect.true
                    (format "Expected {0} to have \"{1}\", but instead it had \"{2}\"."
                        [ holderName
                        , toString expected
                        , toString actual
                        ]
                    )

        Nothing ->
            Expect.fail
                (format "Expected {0} to have \"{1}\", but it was Nothing!"
                    [ holderName
                    , toString expected
                    ]
                )


format : String -> List String -> String
format input list =
    let
        key =
            \i -> "{" ++ String.fromInt i ++ "}"

        folder =
            \value ( i, string ) ->
                ( i + 1
                , string |> String.replace (key i) value
                )

        ( _, newString ) =
            List.foldl folder ( 0, input ) list
    in
    newString
