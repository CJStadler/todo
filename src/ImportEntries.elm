module ImportEntries exposing (filter, update)

{-| Logic for importing entries from a range of dates to the last date.

The range is open on the left and closed on the right.

-}

import Date exposing (Date)
import EntrySchedule exposing (EntrySchedule)


filter : Date -> Date -> List Entry -> List Entry
filter from to entries =
    List.filter (incompleteInRange from to) entries


update : Date -> Date -> List EntrySchedule -> List EntrySchedule
update from to entries =
    -- Move incomplete entries on or after `from` to `to`.
    let
        updateIfInRange e =
            if incompleteInRange from to e then
                Entry.new (Entry.description e) (Entry.id e) to

            else
                e
    in
    List.map updateIfInRange entries


incompleteInRange : Date -> Date -> Entry -> Bool
incompleteInRange from to entry =
    let
        -- Date.isBetween takes an open interval, so we subtract 1 from the to
        -- date.
        openTo =
            Date.add Date.Days -1 to
    in
    not (Entry.completed entry)
        && Date.isBetween from openTo (Entry.date entry)
