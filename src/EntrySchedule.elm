module EntrySchedule exposing (EntrySchedule, decode, encode, forDate)

{-| A schedule by which entries should be shown.

There are several types of entry schedules:

  - Single: A single entry on a given date.
  - Weekly: An entry which occurs on a given day of the week.

The main interface is the `forDate` method, which is used to check whether a
schedule has an entry for a given date.

-}

import Date exposing (Date)
import Entry exposing (Entry)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Time exposing (Weekday(..))


type EntrySchedule
    = EntrySchedule EntryData Schedule


type alias EntryData =
    { id : Id
    , name : String
    }


type alias Id =
    Int


type Schedule
    = Single Date
    | Weekly Weekday


forDate : Date -> EntrySchedule -> Maybe Entry
forDate date (EntrySchedule data schedule) =
    case schedule of
        Single genDate ->
            if date == genDate then
                Just (Entry.new data.name data.id date)

            else
                Nothing

        Weekly day ->
            if Date.weekday date == day then
                Just (Entry.new data.name data.id date)

            else
                Nothing


encode : EntrySchedule -> Encode.Value
encode (EntrySchedule data schedule) =
    Encode.object
        [ ( "id", Encode.int data.id )
        , ( "name", Encode.string data.name )
        , ( "schedule", encodeSchedule schedule )
        ]


decode : Decode.Decoder EntrySchedule
decode =
    let
        buildSchedule id name sched =
            EntrySchedule { id = id, name = name } sched
    in
    Decode.map3 buildSchedule
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        scheduleDecoder


scheduleDecoder : Decode.Decoder Schedule
scheduleDecoder =
    let
        buildSingle rataDie =
            Single (Date.fromRataDie rataDie)

        weeklyDecoder : String -> Decode.Decoder Schedule
        weeklyDecoder dayStr =
            let
                maybeDay =
                    decodeWeekday dayStr

                dayDecoder =
                    case maybeDay of
                        Just day ->
                            Decode.succeed day

                        Nothing ->
                            Decode.fail ("Weekday " ++ dayStr ++ " not recognized")
            in
            Decode.map Weekly dayDecoder

        byType : String -> Decode.Decoder Schedule
        byType typeStr =
            case typeStr of
                "Single" ->
                    Decode.map buildSingle (Decode.field "rataDie" Decode.int)

                "Weekly" ->
                    Decode.field "weekday" Decode.string
                        |> Decode.andThen weeklyDecoder

                _ ->
                    Decode.fail <|
                        "EntrySchedule type "
                            ++ typeStr
                            ++ " is not recognized."
    in
    Decode.field "type" Decode.string
        |> Decode.andThen byType


encodeSchedule : Schedule -> Encode.Value
encodeSchedule schedule =
    case schedule of
        Single date ->
            Encode.object
                [ ( "type", Encode.string "Single" )
                , ( "rataDie", Encode.int (Date.toRataDie date) )
                ]

        Weekly weekday ->
            Encode.object
                [ ( "type", Encode.string "Weekly" )
                , ( "weekday", Encode.string (encodeWeekday weekday) )
                ]


encodeWeekday : Weekday -> String
encodeWeekday weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


decodeWeekday : String -> Maybe Weekday
decodeWeekday str =
    case str of
        "Mon" ->
            Just Mon

        "Tue" ->
            Just Tue

        "Wed" ->
            Just Wed

        "Thu" ->
            Just Thu

        "Fri" ->
            Just Fri

        "Sat" ->
            Just Sat

        "Sun" ->
            Just Sun

        _ ->
            Nothing
