module ImportEntries exposing (filter, update)

{-| Logic for importing entries from a range of dates to the last date.

The range is open on the left and closed on the right.

-}

import Date exposing (Date)
import Entry exposing (Entry)
import EntrySchedule exposing (EntrySchedule)


filter : Date -> Date -> List EntrySchedule -> List Entry
filter from to schedules =
    List.filterMap (EntrySchedule.lastInRange from (previous to)) schedules


update :
    Date
    -> Date
    -> EntrySchedule.Id
    -> List EntrySchedule
    -> ( EntrySchedule.Id, List EntrySchedule )
update from to nextId schedules =
    -- Copy incomplete schedules on or after `from` to `to`.
    let
        new id description =
            EntrySchedule.newSingle id description to

        recUpdate recId updated original =
            case original of
                [] ->
                    ( recId, updated )

                e :: rest ->
                    case EntrySchedule.lastInRange from (previous to) e of
                        Just entry ->
                            recUpdate (recId + 1) (new recId (Entry.description entry) :: updated) rest

                        Nothing ->
                            recUpdate recId updated rest
    in
    recUpdate nextId schedules schedules


previous : Date -> Date
previous date =
    Date.add Date.Days -1 date
