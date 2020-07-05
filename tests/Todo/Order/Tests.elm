module Todo.Order.Tests exposing (..)

import Expect exposing (Expectation)
import Expect.Extra
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Order as Order
import Todo.Urgency as Urgency exposing (Urgency(..))


itemSortingProperties : Test
itemSortingProperties =
    Test.describe "Testing specific properties of item sorting."
        [ Test.fuzz2
            (fuzzItem
                { fuzzImportance = Fuzz.constant Need
                , fuzzUrgency = urgencyFuzzer
                , fuzzDetails = Fuzz.string
                }
            )
            (fuzzItem
                { fuzzImportance = Fuzz.constant Want
                , fuzzUrgency = urgencyFuzzer
                , fuzzDetails = Fuzz.string
                }
            )
            "Testing that importance is the dominant sorting factor."
            (\itemA itemB ->
                let
                    itemOrder =
                        Order.byItemAscending
                            itemA
                            itemB
                in
                itemOrder |> Expect.equal GT
            )
        , Test.fuzz2
            (fuzzItem
                { fuzzImportance = Fuzz.constant Need
                , fuzzUrgency = Fuzz.constant Asap
                , fuzzDetails = Fuzz.string
                }
            )
            (fuzzItem
                { fuzzImportance = Fuzz.constant Need
                , fuzzUrgency = Fuzz.constant Soon
                , fuzzDetails = Fuzz.string
                }
            )
            "Testing that urgency is the secondary sorting factor."
            (\itemA itemB ->
                let
                    itemOrder =
                        Order.byItemAscending
                            itemA
                            itemB
                in
                itemOrder |> Expect.equal GT
            )
        ]


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


fuzzItem :
    { fuzzImportance : Fuzzer Importance
    , fuzzUrgency : Fuzzer Urgency
    , fuzzDetails : Fuzzer String
    }
    -> Fuzzer Item
fuzzItem config =
    Fuzz.map4 Item.create
        config.fuzzImportance
        config.fuzzUrgency
        config.fuzzDetails
        config.fuzzDetails


importanceFuzzer : Fuzzer Importance
importanceFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Need
        , Fuzz.constant Want
        , Fuzz.constant NoImportance
        ]


urgencyFuzzer : Fuzzer Urgency
urgencyFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Asap
        , Fuzz.constant Soon
        , Fuzz.constant Eventually
        , Fuzz.constant Whenever
        ]
