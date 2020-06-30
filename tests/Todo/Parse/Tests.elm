module Todo.Parse.Tests exposing (..)

import Expect exposing (Expectation)
import Expect.Extra
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo.Importance as Importance exposing (Importance(..))
import Todo.Item as Item exposing (Item)
import Todo.Parse as Parse
import Todo.Urgency as Urgency exposing (Urgency(..))


isolatedImportance : Test
isolatedImportance =
    Test.describe "Isolated Importance Tests"
        [ Test.test "Need"
            (\_ ->
                "need to do something"
                    |> Parse.item
                    |> expectImportance Need
            )
        , Test.test "Want"
            (\_ ->
                "want to do something"
                    |> Parse.item
                    |> expectImportance Want
            )
        , Test.test "No Importance"
            (\_ ->
                "do something"
                    |> Parse.item
                    |> expectImportance NoImportance
            )
        ]


isolatedUrgency : Test
isolatedUrgency =
    Test.describe "Isolated Urgency Tests"
        [ Test.test "Asap"
            (\_ ->
                "do something asap"
                    |> Parse.item
                    |> expectUrgency Asap
            )
        , Test.test "Soon"
            (\_ ->
                "do something soon"
                    |> Parse.item
                    |> expectUrgency Soon
            )
        , Test.test "Eventually"
            (\_ ->
                "do something eventually"
                    |> Parse.item
                    |> expectUrgency Eventually
            )
        , Test.test "Whenever"
            (\_ ->
                "do something whenever"
                    |> Parse.item
                    |> expectUrgency Whenever
            )
        , Test.test "No Urgency"
            (\_ ->
                "do something"
                    |> Parse.item
                    |> expectUrgency Whenever
            )
        ]


expectImportance : Importance -> Maybe Item -> Expectation
expectImportance =
    Expect.Extra.has "Item"
        Item.getImportance
        Importance.toDisplayString


expectUrgency : Urgency -> Maybe Item -> Expectation
expectUrgency =
    Expect.Extra.has "Item"
        Item.getUrgency
        Urgency.toDisplayString


expectEquals : String -> Item -> Maybe Item -> Expectation
expectEquals failMsg expected actual =
    actual
        |> Maybe.map
            (Item.equals expected
                >> Expect.true failMsg
            )
        |> Maybe.withDefault (Expect.fail failMsg)
