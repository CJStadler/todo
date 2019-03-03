module EntryScheduleTests exposing (suite)

import Date exposing (Date)
import EntrySchedule exposing (encode)
import Expect exposing (Expectation)
import Fuzz exposing (int, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Test exposing (..)


suite : Test
suite =
    let
        dateFuzzer =
            Fuzz.map Date.fromRataDie (Fuzz.intRange 0 Random.maxInt)

        weekdayFuzzer =
            Fuzz.map Date.numberToWeekday (Fuzz.intRange 1 7)
    in
    describe "EntrySchedule module"
        [ describe "encode and decode"
            [ describe "Single schedule"
                [ fuzz3 int string dateFuzzer "produces expected JSON value" <|
                    \id desc date ->
                        let
                            schedule =
                                EntrySchedule.newSingle id desc date
                        in
                        EntrySchedule.encode schedule
                            |> Decode.decodeValue EntrySchedule.decode
                            |> Result.map (Expect.equal schedule)
                            |> Result.withDefault (Expect.fail "Decoding error")
                ]
            , describe "Weekly schedule"
                [ fuzz3 int string weekdayFuzzer "produces expected JSON value" <|
                    \id desc weekday ->
                        let
                            schedule =
                                EntrySchedule.newWeekly id desc weekday
                        in
                        EntrySchedule.encode schedule
                            |> Decode.decodeValue EntrySchedule.decode
                            |> Result.map (Expect.equal schedule)
                            |> Result.withDefault (Expect.fail "Decoding error")
                ]
            ]
        ]
