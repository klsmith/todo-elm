module Todo.Order.Tests exposing (..)

import Expect exposing (Expectation)
import Expect.Extra
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Order as Order
import Todo.Urgency as Urgency exposing (Urgency(..))


exhaustiveListTests : Test
exhaustiveListTests =
    Test.describe "Testing ordering by using exhaustive lists where possible."
        [ Test.test "Testing Importance Ascending"
            (\_ ->
                let
                    input =
                        [ NoImportance
                        , Want
                        , Need
                        ]
                in
                input
                    |> List.sortWith Order.byImportanceAscending
                    |> Expect.equalLists input
            )
        , Test.test "Testing Importance Descending"
            (\_ ->
                let
                    input =
                        [ Need
                        , Want
                        , NoImportance
                        ]
                in
                input
                    |> List.sortWith Order.byImportanceDescending
                    |> Expect.equalLists input
            )
        , Test.test "Testing Urgency Ordering Ascending"
            (\_ ->
                let
                    input =
                        [ Whenever
                        , Eventually
                        , Soon
                        , Asap
                        ]
                in
                input
                    |> List.sortWith Order.byUrgencyAscending
                    |> Expect.equalLists input
            )
        , Test.test "Testing Urgency Ordering Descending"
            (\_ ->
                let
                    input =
                        [ Asap
                        , Soon
                        , Eventually
                        , Whenever
                        ]
                in
                input
                    |> List.sortWith Order.byUrgencyDescending
                    |> Expect.equalLists input
            )
        ]
