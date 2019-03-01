module EntrySchedule exposing
    ( EntrySchedule
    , decode
    , encode
    , forDate
    , lastInRange
    , newSingle
    , newWeekly
    )

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
    = Single Date Bool
    | Weekly Weekday (List Date) -- Completed dates


{-| Mark a schedule as completed on a certain date.
-}
complete : Date -> Bool -> EntrySchedule -> EntrySchedule
complete completedDate newCompleted (EntrySchedule data schedule) =
    let
        newSchedule =
            case schedule of
                Single date completed ->
                    if completedDate == date then
                        Single date newCompleted

                    else
                        schedule

                Weekly day completedDates ->
                    if newCompleted then
                        Weekly day (completedDate :: completedDates)

                    else
                        Weekly day (List.filter ((/=) completedDate) completedDates)
    in
    EntrySchedule data newSchedule


{-| Change the text of a schedule
-}
edit : String -> EntrySchedule -> EntrySchedule
edit text (EntrySchedule data schedule) =
    EntrySchedule { data | name = text } schedule


{-| Get an entry from a schedule for a given date, if there is one.
-}
forDate : Date -> EntrySchedule -> Maybe Entry
forDate date (EntrySchedule data schedule) =
    case schedule of
        Single genDate completed ->
            if date == genDate then
                Just
                    (Entry.update (Entry.Completed True) (Entry.new data.name data.id date))

            else
                Nothing

        Weekly day completedDates ->
            if Date.weekday date == day then
                let
                    entry =
                        Entry.new data.name data.id date

                    updated =
                        if List.member date completedDates then
                            Entry.update (Entry.Completed True) entry

                        else
                            entry
                in
                Just updated

            else
                Nothing


{-| Get the latest entry in the date range.
-}
lastInRange : Date -> Date -> EntrySchedule -> Maybe Entry
lastInRange from to schedule =
    if Date.compare to from == LT then
        Nothing

    else
        -- Search dates backwards from `to` to `from`.
        case forDate to schedule of
            Just entry ->
                Just entry

            Nothing ->
                lastInRange from (Date.add Date.Days -1 to) schedule


newWeekly : Id -> String -> Weekday -> EntrySchedule
newWeekly id name day =
    EntrySchedule { id = id, name = name } (Weekly day [])


newSingle : Id -> String -> Date -> EntrySchedule
newSingle id name date =
    EntrySchedule { id = id, name = name } (Single date False)


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
        buildSingle rataDie completed =
            Single (Date.fromRataDie rataDie) completed

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

                completedDecoder =
                    Decode.map (List.map Date.fromRataDie) (Decode.field "completedRataDie" (Decode.list Decode.int))
            in
            Decode.map2 Weekly dayDecoder completedDecoder

        byType : String -> Decode.Decoder Schedule
        byType typeStr =
            case typeStr of
                "Single" ->
                    Decode.map2 buildSingle
                        (Decode.field "rataDie" Decode.int)
                        (Decode.field "completed" Decode.bool)

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
        Single date completed ->
            Encode.object
                [ ( "type", Encode.string "Single" )
                , ( "rataDie", Encode.int (Date.toRataDie date) )
                , ( "completed", Encode.bool completed )
                ]

        Weekly weekday completedList ->
            Encode.object
                [ ( "type", Encode.string "Weekly" )
                , ( "weekday", Encode.string (encodeWeekday weekday) )
                , ( "completedRataDie"
                  , Encode.list Encode.int
                        (List.map Date.toRataDie completedList)
                  )
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
