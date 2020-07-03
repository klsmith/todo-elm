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
                "need something"
                    |> Parse.item
                    |> Item.getImportance
                    |> Expect.equal Need
            )
        , Test.test "Want"
            (\_ ->
                "want something"
                    |> Parse.item
                    |> Item.getImportance
                    |> Expect.equal Want
            )
        , Test.test "No Importance"
            (\_ ->
                "something"
                    |> Parse.item
                    |> Item.getImportance
                    |> Expect.equal NoImportance
            )
        ]


isolatedUrgency : Test
isolatedUrgency =
    Test.describe "Isolated Urgency Tests"
        [ Test.test "Asap"
            (\_ ->
                "something asap"
                    |> Parse.item
                    |> Item.getUrgency
                    |> Expect.equal Asap
            )
        , Test.test "Soon"
            (\_ ->
                "something soon"
                    |> Parse.item
                    |> Item.getUrgency
                    |> Expect.equal Soon
            )
        , Test.test "Eventually"
            (\_ ->
                "something eventually"
                    |> Parse.item
                    |> Item.getUrgency
                    |> Expect.equal Eventually
            )
        , Test.test "Whenever"
            (\_ ->
                "something whenever"
                    |> Parse.item
                    |> Item.getUrgency
                    |> Expect.equal Whenever
            )
        , Test.test "No Urgency"
            (\_ ->
                "something"
                    |> Parse.item
                    |> Item.getUrgency
                    |> Expect.equal Whenever
            )
        ]


specifics : Test
specifics =
    Test.describe "Specific Parse Cases"
        [ Test.test "Test \"to\" is NOT considered details."
            (\_ ->
                "need to do something"
                    |> Parse.item
                    |> Item.getDetails
                    |> Expect.equal "do something"
            )
        , Test.test "Test \"to\" IS considered part of details."
            (\_ ->
                "to do something"
                    |> Parse.item
                    |> Item.getDetails
                    |> Expect.equal "to do something"
            )
        ]
