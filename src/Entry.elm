module Entry exposing
    ( Entry
    , Id
    , Serialized
    , Update(..)
    , completed
    , date
    , description
    , deserialize
    , id
    , new
    , onDate
    , serialize
    , update
    )

import Date exposing (Date)


type Entry
    = Entry Record


type alias Id =
    Int


type Update
    = Description String
    | Completed Bool


type alias Record =
    { description : String
    , completed : Bool
    , id : Id
    , date : Date
    }


type alias Serialized =
    { description : String
    , completed : Bool
    , id : Int
    , date : Int
    }


new : String -> Id -> Date -> Entry
new desc eid d =
    Entry
        { description = desc
        , completed = False
        , id = eid
        , date = d
        }


description : Entry -> String
description (Entry r) =
    r.description


completed : Entry -> Bool
completed (Entry r) =
    r.completed


id : Entry -> Id
id (Entry r) =
    r.id


date : Entry -> Date
date (Entry r) =
    r.date


onDate : Date -> Entry -> Bool
onDate d entry =
    Date.compare d (date entry) == EQ


update : Update -> Entry -> Entry
update change (Entry r) =
    case change of
        Description d ->
            Entry { r | description = d }

        Completed c ->
            Entry { r | completed = c }


serialize : Entry -> Serialized
serialize (Entry r) =
    { description = r.description
    , completed = r.completed
    , id = r.id
    , date = Date.toRataDie r.date
    }


deserialize : Serialized -> Entry
deserialize e =
    Entry
        { description = e.description
        , completed = e.completed
        , id = e.id
        , date = Date.fromRataDie e.date
        }
