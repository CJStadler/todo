module ImportEntries exposing (filter, update)

{-| Logic for importing entries from a range of dates to the last date.

The range is open on the left and closed on the right.

-}

import Date exposing (Date)
import Entry exposing (Entry)
import EntrySchedule exposing (EntrySchedule)


filter : Date -> Date -> List EntrySchedule -> List Entry
filter from to schedules =
    List.filterMap (EntrySchedule.lastInRange from to) schedules


update : Date -> Date -> List EntrySchedule -> List EntrySchedule
update from to entries =
    -- Copy incomplete entries on or after `from` to `to`.
    let
        helper updated original =
            case original of
                [] ->
                    updated

                e :: rest ->
                    case EntrySchedule.lastInRange from to e of
                        Just entry ->
                            singleFromEntry entry :: updated

                        Nothing ->
                            updated
    in
    helper entries entries


singleFromEntry : Entry -> EntrySchedule
singleFromEntry e =
    -- TODO: How to get ID?
    EntrySchedule.newSingle 123 (Entry.description e) (Entry.date e)
