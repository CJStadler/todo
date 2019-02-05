module Entry exposing
    ( Entry
    , Serialized
    , Update(..)
    , completed
    , date
    , description
    , deserialize
    , editing
    , id
    , new
    , serialize
    , update
    )

import Date exposing (Date)


type Entry
    = Entry Record


type Update
    = Description String
    | Completed Bool
    | Editing Bool


type alias Record =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , date : Date
    }


type alias Serialized =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , date : Date.Serialized
    }


new : String -> Int -> Date -> Entry
new desc uid d =
    Entry
        { description = desc
        , completed = False
        , editing = False
        , id = uid
        , date = d
        }


description : Entry -> String
description (Entry r) =
    r.description


completed : Entry -> Bool
completed (Entry r) =
    r.completed


editing : Entry -> Bool
editing (Entry r) =
    r.editing


id : Entry -> Int
id (Entry r) =
    r.id


date : Entry -> Date
date (Entry r) =
    r.date


update : Update -> Entry -> Entry
update change (Entry r) =
    case change of
        Description d ->
            Entry { r | description = d }

        Completed c ->
            Entry { r | completed = c }

        Editing e ->
            Entry { r | editing = e }


serialize : Entry -> Serialized
serialize (Entry r) =
    { description = r.description
    , completed = r.completed
    , editing = r.editing
    , id = r.id
    , date = Date.serialize r.date
    }


deserialize : Serialized -> Entry
deserialize e =
    Entry
        { description = e.description
        , completed = e.completed
        , editing = e.editing
        , id = e.id
        , date = Date.deserialize e.date
        }
